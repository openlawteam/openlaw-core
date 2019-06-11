package org.adridadou.openlaw.parser.template

import cats.implicits._
import cats.Eq
import java.time.{Clock, ZoneId}
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.adridadou.openlaw.values.{ContractId, TemplateParameters, TemplateTitle}
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.{OpenlawMap, OpenlawValue}
import org.adridadou.openlaw.oracles.OpenlawSignatureProof
import org.adridadou.openlaw.result.{Failure, FailureCause, Result, ResultNel, Success}
import org.adridadou.openlaw.result.Implicits.{RichResult, RichResultNel}
import org.adridadou.openlaw.vm.Executions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import VariableName._

trait TemplateExecutionResult {
  def id:TemplateExecutionResultId
  def clock:Clock
  def templateDefinition:Option[TemplateDefinition]
  def subExecutions:Map[VariableName, TemplateExecutionResult]
  def signatureProofs:Map[Email, OpenlawSignatureProof]
  def parentExecution:Option[TemplateExecutionResult]
  def variables:Seq[VariableDefinition]
  def mapping:Map[VariableName, Expression]
  def aliases:Seq[VariableAliasing]
  def sectionNameMappingInverse:Map[VariableName, String]
  def variableTypes:Seq[VariableType]
  def variableSections:Map[String, Seq[VariableName]]
  def parameters:TemplateParameters
  def embedded:Boolean
  def processedSections:Seq[(Section, Int)]
  def executedVariables:Seq[VariableName]
  def agreements:Seq[StructuredAgreement]
  def variableSectionList:Seq[String]
  def executions:Map[VariableName, Executions]
  def info:OLInformation
  def hasSigned(email: Email):Boolean =
    if(signatureProofs.contains(email)) true else parentExecution.exists(_.hasSigned(email))

  def findExecutionResult(executionResultId: TemplateExecutionResultId): Option[TemplateExecutionResult] = {
    if(id === executionResultId) {
      Some(this)
    } else {
      subExecutions.values.flatMap(_.findExecutionResult(executionResultId)).headOption
    }
  }

  def getVariable(variable:VariableDefinition):Option[VariableDefinition] =
    getVariable(variable.name)

  def getVariable(name:VariableName):Option[VariableDefinition] = {
    if(this.sectionNameMappingInverse.contains(name)) {
      this.variables.find(definition => definition.name === name && definition.varType(this) === SectionType)
    } else {
      (mapping.get(name) match {
        case Some(_) =>
          None
        case None =>
          variables.find(_.name === name)
      }) match {
        case Some(variable) =>
          Some(variable)
        case None =>
          parentExecution
            .flatMap(_.getVariable(name))
      }
    }
  }

  def getVariable(name:String):Option[VariableDefinition] =
    getVariable(VariableName(name))

  def getAlias(name:VariableName):Option[Expression] =
    (mapping.get(name) match {
      case Some(expression) =>
        parentExecution.map(MappingExpression(expression,_))
      case None =>
        aliases.find(_.name === name)
    }) match {
      case Some(alias) =>
        Some(alias)
      case None =>
        parentExecution
          .flatMap(_.getAlias(name))
    }

  def getAlias(name:String):Option[Expression] =
    getAlias(VariableName(name))

  def getAliasOrVariableType(name:VariableName): Result[VariableType] = {
    getExpression(name)
      .map(_.expressionType(this))
      .getOrElse(Failure(s"${name.name} cannot be resolved!"))
  }

  def getVariables:Seq[VariableDefinition] =
    variables
      .filter(variable => !mapping.contains(variable.name))
      .foldLeft(DistinctVariableBuilder())((builder,variable) => if(builder.names.contains(variable.name)) {
        builder
      } else {
        builder.add(variable)
      }).variables

  def getVariables(varType: VariableType):Seq[(TemplateExecutionResult, VariableDefinition)] =
    getVariables
      .filter(variable => {
        variable.varType(this) match {
          case collectionType:CollectionType =>
            collectionType.typeParameter === varType
          case structuredType:DefinedStructureType =>
            structuredType.structure.typeDefinition.values.exists(_ === varType)
          case variableType => variableType === varType
        }}).map((this, _)) ++ subExecutions.values.flatMap(_.getVariables(varType))

  def getVariableValues[U <: OpenlawValue](varType: VariableType)(implicit classTag:ClassTag[U]):Result[Seq[U#T]] =
    getVariables(varType)
      .map { case (execution, variable) =>
        variable.evaluate(execution).flatMap(_.map(getVariableValue[U](_, variable.varType(this))).sequence)
      }
      .toList
      .sequence
      .map(_.flatten)

  def getVariableValue[U <: OpenlawValue](name: VariableName)(implicit classTag:ClassTag[U]):Result[Option[U#T]] =
    getVariable(name)
      .map(variable => variable.evaluate(this).flatMap(_.map(getVariableValue[U](_, variable.varType(this))).sequence))
      .sequence
      .map(_.flatten)

  def getVariableValue[U <: OpenlawValue](value:OpenlawValue, variableType:VariableType)(implicit classTag: ClassTag[U]): Result[U#T] = VariableType.convert[U](value)

  def getParameter(name: String):Option[String] = getParameter(VariableName(name))

  def getParameter(name: VariableName):Option[String] = parameters.get(name) match {
    case Some(value) =>
      Some(value)
    case None =>
      parentExecution.flatMap(_.getParameter(name))
  }

  def allIdentityEmails: Result[Seq[Email]] = allIdentities().map(_.map(_.email).distinct)

  def allIdentities(): Result[Seq[Identity]] = {
   getAllExecutedVariables
      .flatMap { case (result, name) => result.getVariable(name).map(variable => (result, variable)) }
      .map { case (result, variable) =>
        variable.varType(result) match {
          case IdentityType =>
            variable.evaluate(result).flatMap(_.map(VariableType.convert[Identity]).sequence).map(_.toSeq)
          case collectionType: CollectionType if collectionType.typeParameter === IdentityType =>
            variable
              .evaluate(result)
              .map { option =>
                option.map(VariableType.convert[CollectionValue](_).flatMap(x => x.list.map(VariableType.convert[Identity]).toList.sequence)).sequence
              }
              .flatten
              .map(_.sequence.flatten)
          case structureType: DefinedStructureType if structureType.structure.typeDefinition.values.exists(_ === IdentityType) =>
            variable
              .evaluate(result)
              .flatMap(_.map(VariableType.convert[OpenlawMap[VariableName, OpenlawValue]](_)).sequence)
              .map(_.getOrElse(Map()))
              .flatMap { values =>
                structureType
                  .structure
                  .names
                  .filter(name => structureType.structure.typeDefinition(name) === IdentityType)
                  .map(name => VariableType.convert[Identity](values(name)))
                  .toList
                  .sequence
              }

          case _ =>
            Success(Seq())
        }
      }
       .toList
       .sequence
       .map(_.flatten)
  }

  def allActions(): Result[Seq[ActionInfo]] =
    getAllExecutedVariables.map {
      case (executionResult, variableName) =>
        executionResult
          .getVariable(variableName)
          .map { variable =>
            (variable.varType(executionResult) match {
              case actionType: ActionType =>
                variable.evaluate(executionResult).flatMap(_.map(actionType.actionValue).sequence)
              case _ =>
                Success(None)
            })
            .map(x => x.map(action => ActionInfo(action, variableName, executionResult)).toList)
          }
          .toList
          .sequence
          .map(_.flatten)
    }
    .toList
    .sequence
    .map(_.flatten)

  def getTemplateDefinitionForVariable(name: VariableName):Option[TemplateDefinition] = variables.find(_.name === name) match {
    case Some(_) =>
      templateDefinition
    case None =>
      parentExecution.flatMap(_.getTemplateDefinitionForVariable(name))
  }

  def allProcessedSections:Seq[(Section, Int)] = (embedded, parentExecution)  match {
    case (true, Some(parent)) => parent.allProcessedSections ++ processedSections
    case _ => processedSections
  }

  def getAllExecutedVariables:Seq[(TemplateExecutionResult, VariableName)] =
    executedVariables.distinct.map(name => (this, name)) ++ subExecutions.values.flatMap(_.getAllExecutedVariables)


  @tailrec
  final def getAllVariables:Seq[(TemplateExecutionResult, VariableDefinition)] = {
    parentExecution match {
      case Some(execution) =>
        execution.getAllVariables
      case None =>
        getAllVariablesFromRoot
    }
  }

  private def getAllVariablesFromRoot:Seq[(TemplateExecutionResult, VariableDefinition)] = {
    getVariables.map(variable => (this, variable)) ++ subExecutions.values.flatMap(_.getAllVariablesFromRoot)
  }

  def getVariableNames:Seq[VariableName] = getVariables.foldLeft(DistinctVariableBuilder())({case (builder, variable) => if(builder.names.contains(variable.name)) {
    builder
  } else {
    builder.add(variable)
  }}).variables.map(_.name)

  def getAllVariableNames:Seq[VariableName] = getAllVariables.foldLeft(DistinctVariableBuilder())({case (builder, (_, variable)) => if(builder.names.contains(variable.name)) {
    builder
  } else {
    builder.add(variable)
  }}).variables.map(_.name)

  def findVariableType(variableTypeDefinition: VariableTypeDefinition):Option[VariableType] = {
    val mainType = findVariableTypeInternal(variableTypeDefinition)
    val parameterType = variableTypeDefinition.typeParameter.flatMap(findVariableTypeInternal)
    mainType match {
      case Some(varType:ParameterTypeProvider) =>
        parameterType.map(varType.createParameterInstance)
      case other =>
        other
    }
  }

  private def findVariableTypeInternal(variableTypeDefinition: VariableTypeDefinition):Option[VariableType] =
    variableTypes.find(_.checkTypeName(variableTypeDefinition.name)) match {
      case Some(variableType) =>
        Some(variableType)
      case None =>
        parentExecution
          .flatMap(_.findVariableType(variableTypeDefinition))
    }

  def getSignatureProof(identity: Identity):Option[SignatureProof] = signatureProofs.get(identity.email) match {
    case Some(value) => Some(value)
    case None => parentExecution.flatMap(_.getSignatureProof(identity))
  }

  def sections:Map[String, Seq[VariableName]] = variableSections
    .map({
      case (name, sectionVariables) => name -> sectionVariables.distinct
    })

  def getExpression(name: VariableName):Option[Expression] = getAlias(name) match {
    case Some(alias) =>
      Some(alias)
    case None =>
      getVariable(name)
  }

  def validate: ResultNel[Unit] =
    getVariableValues[Validation](ValidationType).toResultNel andThen { values =>
      ResultNel(values.toList.map(x => x.validate(this))).toUnit
    }


  def getExecutedVariables:Seq[VariableName] = {
    val variableNames = getAllVariableNames
    getAllExecutedVariables.
      filter({case (_, variable) => variableNames.contains(variable)})
     .map({case (_, name) => name}).distinct
  }

  def getAllExecutionResults:Seq[TemplateExecutionResult] =
    subExecutions.values.flatMap(_.getAllExecutionResults).toSeq ++ Seq(this)

  def startEphemeralExecution(name:VariableName, value:OpenlawValue, varType:VariableType): Result[TemplateExecutionResult] = {
    this.getAliasOrVariableType(name) match {
      case Success(_) =>
        Failure(s"${name.name} has already been defined!")
      case Failure(_,_) =>
        varType.internalFormat(value).map { internalFormat =>
          val result = OpenlawExecutionState(
            id = TemplateExecutionResultId(UUID.randomUUID().toString),
            info = info,
            embedded = true,
            parameters = TemplateParameters(name.name -> internalFormat),
            sectionLevelStack = mutable.Buffer(),
            template = CompiledAgreement(header = TemplateHeader()),
            anonymousVariableCounter = new AtomicInteger(0),
            clock = clock,
            parentExecution = Some(this),
            executions = this.executions,
            variableRedefinition = VariableRedefinition()
          )

          result.registerNewType(varType)

          result.variablesInternal.append(VariableDefinition(name, Some(VariableTypeDefinition(name = varType.name, None))))

          result.executedVariablesInternal.append(name)

          result
        }
    }
  }
}

object SerializableTemplateExecutionResult {
  implicit val serializableTemplateExecutionResultEnc:Encoder[SerializableTemplateExecutionResult] = deriveEncoder[SerializableTemplateExecutionResult]
  implicit val serializableTemplateExecutionResultDec:Decoder[SerializableTemplateExecutionResult] = deriveDecoder[SerializableTemplateExecutionResult]

  implicit val clockEnc:Encoder[Clock] = (a: Clock) => Json.fromString(a.getZone.getId)
  implicit val clockDec:Decoder[Clock] = (c: HCursor) => c.as[String].map(id => Clock.system(ZoneId.of(id)))
}

case class SerializableTemplateExecutionResult(id:TemplateExecutionResultId,
                                               info:OLInformation,
                                               templateDefinition: Option[TemplateDefinition] = None,
                                               subExecutionIds:Map[VariableName, TemplateExecutionResultId],
                                               templateExecutions:Map[TemplateExecutionResultId, SerializableTemplateExecutionResult],
                                               executions:Map[VariableName, Executions],
                                               parentExecutionId:Option[TemplateExecutionResultId],
                                               agreements:Seq[StructuredAgreement],
                                               variableSectionList:Seq[String],
                                               signatureProofs:Map[Email, OpenlawSignatureProof],
                                               variables:Seq[VariableDefinition],
                                               executedVariables:Seq[VariableName],
                                               mapping:Map[VariableName, Expression],
                                               aliases:Seq[VariableAliasing],
                                               sectionNameMappingInverse:Map[VariableName, String],
                                               variableTypes:Seq[VariableType],
                                               variableSections:Map[String, Seq[VariableName]],
                                               parameters:TemplateParameters,
                                               embedded:Boolean,
                                               processedSections:Seq[(Section, Int)],
                                               clock:Clock) extends TemplateExecutionResult {

  override def subExecutions: Map[VariableName, TemplateExecutionResult] = subExecutionIds.flatMap({case (name, executionId) => templateExecutions.get(executionId).map(name -> _)})
  override def parentExecution: Option[TemplateExecutionResult] = parentExecutionId.flatMap(templateExecutions.get)
}


case class OpenlawExecutionState(
                                    id:TemplateExecutionResultId,
                                    parameters:TemplateParameters,
                                    embedded:Boolean,
                                    info:OLInformation,
                                    executions:Map[VariableName,Executions],
                                    signatureProofs:Map[Email, OpenlawSignatureProof] = Map(),
                                    template:CompiledTemplate,
                                    forEachQueue:mutable.Buffer[Any] = mutable.Buffer(),
                                    anonymousVariableCounter:AtomicInteger,
                                    variablesInternal:mutable.Buffer[VariableDefinition] = mutable.Buffer(),
                                    aliasesInternal:mutable.Buffer[VariableAliasing] = mutable.Buffer(),
                                    executedVariablesInternal:mutable.Buffer[VariableName] = mutable.Buffer(),
                                    variableSectionsInternal:mutable.Map[String, mutable.Buffer[VariableName]] = mutable.Map(),
                                    variableSectionListInternal:mutable.Buffer[String] = mutable.Buffer(),
                                    agreementsInternal:mutable.Buffer[StructuredAgreement] = mutable.Buffer(),
                                    subExecutionsInternal:mutable.Map[VariableName, OpenlawExecutionState] = mutable.Map(),
                                    forEachExecutions:mutable.Buffer[OpenlawExecutionState] = mutable.Buffer(),
                                    finishedEmbeddedExecutions:mutable.Buffer[OpenlawExecutionState] = mutable.Buffer(),
                                    state:TemplateExecutionState = ExecutionReady,
                                    remainingElements:mutable.Buffer[TemplatePart] = mutable.Buffer(),
                                    parentExecution:Option[TemplateExecutionResult] = None,
                                    parentExecutionInternal:Option[OpenlawExecutionState] = None,
                                    compiledAgreement:Option[CompiledAgreement] = None,
                                    variableRedefinition: VariableRedefinition,
                                    templateDefinition: Option[TemplateDefinition] = None,
                                    mapping:Map[VariableName, Expression] = Map(),
                                    variableTypesInternal: mutable.Buffer[VariableType] = mutable.Buffer(VariableType.allTypes() : _*),
                                    sectionLevelStack: mutable.Buffer[Int] = mutable.Buffer(),
                                    sectionNameMapping: mutable.Map[String, VariableName] = mutable.Map(),
                                    sectionNameMappingInverseInternal: mutable.Map[VariableName, String] = mutable.Map(),
                                    processedSectionsInternal: mutable.Buffer[(Section, Int)] = mutable.Buffer(),
                                    lastSectionByLevel:mutable.Map[Int, String] = mutable.Map(),
                                    clock:Clock) extends TemplateExecutionResult {

  def variables:Seq[VariableDefinition] = variablesInternal
  def aliases:Seq[VariableAliasing] = aliasesInternal
  def variableTypes:Seq[VariableType] = variableTypesInternal
  def processedSections: Seq[(Section, Int)] = processedSectionsInternal
  def executedVariables:Seq[VariableName] = executedVariablesInternal
  def agreements:Seq[StructuredAgreement] = agreementsInternal
  def variableSectionList:Seq[String] = variableSectionListInternal

  override def variableSections: Map[String, Seq[VariableName]] = variableSectionsInternal.toMap

  override def sectionNameMappingInverse: Map[VariableName, String] = sectionNameMappingInverseInternal.toMap

  def addLastSectionByLevel(lvl: Int, sectionValue: String):Unit = {
    if(embedded) {
      parentExecutionInternal match {
        case Some(parent) => parent.addLastSectionByLevel(lvl, sectionValue)
        case None => lastSectionByLevel put (lvl , sectionValue)
      }
    } else {
      lastSectionByLevel put (lvl , sectionValue)
    }
  }

  override def subExecutions: Map[VariableName, TemplateExecutionResult] = subExecutionsInternal.toMap

  def getLastSectionByLevel(idx: Int): String = {
    if(embedded) {
      parentExecutionInternal match {
        case Some(parent) => parent.getLastSectionByLevel(idx)
        case None => lastSectionByLevel.getOrElse(idx,"")
      }
    } else {
      lastSectionByLevel.getOrElse(idx,"")
    }
  }

  def addProcessedSection(section: Section, number: Int):Unit = (embedded, parentExecutionInternal) match {
    case (true, Some(parent)) => parent.addProcessedSection(section, number)
    case _ => processedSectionsInternal append (section -> number)
  }

  def addSectionLevelStack(newSectionValues: Seq[Int]):Unit = {
    if(embedded) {
      parentExecutionInternal match {
        case Some(parent) => parent.addSectionLevelStack(newSectionValues)
        case None => sectionLevelStack appendAll newSectionValues
      }
    } else {
      sectionLevelStack appendAll newSectionValues
    }
  }

  def allSectionLevelStack:Seq[Int] = {
    if(embedded) {
      parentExecutionInternal match {
        case Some(parent) => parent.allSectionLevelStack ++ sectionLevelStack
        case None => sectionLevelStack
      }
    } else {
      sectionLevelStack
    }
  }

  def executionLevel:Int = executionLevel(parentExecution, 0)

  def validateExecution: Result[ValidationResult] = {
    val variables = getAllExecutedVariables
      .flatMap({case (result, name) => result.getVariable(name).map(variable => (result, variable))})
      .filter({case (_, variable) => variable.varType(this) match {
        case _:NoShowInForm => false
        case _ => true
      }})

    val identities = variables.filter({ case (result, variable) =>
      variable.varType(result) match {
        case IdentityType => true
        case collectionType:CollectionType if collectionType.typeParameter === IdentityType => true
        case structureType:DefinedStructureType if structureType.structure.typeDefinition.values.exists(_ === IdentityType) => true
        case _ => false
      }
    }).map({case (_, variable) => variable})

    val missingIdentitiesResult = {
      variables.map { case (result, variable) =>
        variable.varType(result) match {
          case IdentityType =>
            Success(resultFromMissingInput(variable.missingInput(result)))

          case collectionType: CollectionType if collectionType.typeParameter === IdentityType =>
            result.getVariableValue[CollectionValue](variable.name).map {
              case Some(value) if value.size =!= value.values.size =>
                (Seq(variable.name), Seq())
              case Some(_) =>
                (Seq(), Seq())
              case None =>
                (Seq(variable.name), Seq())
            }

          case structureType: DefinedStructureType if structureType.structure.typeDefinition.values.exists(_ === IdentityType) =>
            result.getVariableValue[OpenlawMap[VariableName, OpenlawValue]](variable.name).map { values =>
              val identityProperties = structureType.structure.typeDefinition
                .filter({ case (_, propertyType) => propertyType === IdentityType })
                .map({ case (propertyName, _) => propertyName }).toSeq

              if (identityProperties.forall(values.getOrElse(Map()).contains)) {
                (Seq(), Seq())
              } else {
                (Seq(variable.name), Seq())
              }
            }

          case _ =>
            Success((Seq(), Seq()))
        }
      }
      .toList
      .sequence
    }

    missingIdentitiesResult.map { missingIdentitiesValue =>

      val identitiesErrors = missingIdentitiesValue.flatMap({
        case (_, errors) => errors
      })

      val missingIdentities = missingIdentitiesValue.flatMap({
        case (values, _) => values
      })

      val (missingInputs, additionalErrors) = resultFromMissingInput(allMissingInput)

      val validationErrors = validate.leftMap(nel => nel.map(_.message).toList).swap.getOrElse(Nil)

      ValidationResult(
        identities = identities,
        missingInputs = missingInputs,
        missingIdentities = missingIdentities,
        validationExpressionErrors = validationErrors ++ additionalErrors ++ identitiesErrors
      )
    }
  }

  def toSerializable:SerializableTemplateExecutionResult = {
    val templateExecutions = getSubExecutions.map(e => e.id -> e.toSerializable).toMap
    val subExecutionIds = subExecutions.map({ case (name, execution) => name -> execution.id})

    SerializableTemplateExecutionResult(
      id = id,
      info = info,
      templateDefinition = templateDefinition,
      subExecutionIds = subExecutionIds,
      templateExecutions = templateExecutions,
      executions = executions,
      agreements = agreements,
      variableSectionList = variableSectionList,
      parentExecutionId = parentExecution.map(_.id),
      signatureProofs = signatureProofs,
      variables = variables,
      executedVariables = executedVariables,
      mapping = mapping,
      aliases = aliases,
      sectionNameMappingInverse = sectionNameMappingInverse,
      variableTypes = variableTypes,
      variableSections = variableSections,
      parameters = parameters,
      embedded = embedded,
      processedSections = processedSections,
      clock = clock
    )
  }

  private def getSubExecutions:Seq[OpenlawExecutionState] = subExecutionsInternal.values.toSeq

  private def resultFromMissingInput(seq:Result[Seq[VariableName]]): (Seq[VariableName], Seq[String]) = seq match {
    case Right(inputs) => (inputs, Seq())
    case Left(ex) => (Seq(), Seq(ex.message))
  }

  private def executionLevel(parent:Option[TemplateExecutionResult], acc:Int):Int =
    parent.map(p => executionLevel(p.parentExecution, acc + 1)).getOrElse(acc)

  def allMissingInput: Result[Seq[VariableName]] = {
    val missingInputs = getAllExecutedVariables.filter({
      case (_, _:NoShowInForm) => false
      case _ => true
    }).map({case (result, variable) => variable.missingInput(result)})

    VariableType.sequence(missingInputs).map(_.flatten.distinct)
  }

  def registerNewType(variableType: VariableType): Result[OpenlawExecutionState] = {
    findVariableType(VariableTypeDefinition(variableType.name)) match {
      case None =>
        variableTypesInternal append variableType
        Success(this)
      case Some(_) =>
        Failure(s"the variable type ${variableType.name} as already been defined")
    }
  }

  def notFinished: Boolean = state match {
    case ExecutionFinished => false
    case _ => true
  }

  def createAnonymousVariable():VariableName = {
    this.parentExecutionInternal.map(_.createAnonymousVariable()) match {
      case Some(name) => name
      case None =>
        val currentCounter = anonymousVariableCounter.incrementAndGet()
        VariableName(s"@@anonymous_$currentCounter@@")
    }
  }

  def getTemplateIdentifier:Option[TemplateSourceIdentifier] = state match {
    case ExecutionWaitForTemplate(_, identifier, _) =>
      Some(identifier)
    case _ => None
  }

  def getCompiledTemplate(templates: Map[TemplateSourceIdentifier, CompiledTemplate]):Option[CompiledTemplate] =
    getTemplateIdentifier flatMap templates.get

  def startForEachExecution(variableName:VariableName, template:CompiledTemplate, name:VariableName, value:OpenlawValue, varType:VariableType): Result[OpenlawExecutionState] =
    for {
      result <- startSubExecution(variableName, template, embedded = true)
      internalFormat <- varType.internalFormat(value)
    } yield {
      val newResult = result.copy(parameters = parameters + (name -> internalFormat))
      this.forEachExecutions append newResult
      newResult
    }

  def startSubExecution(variableName:VariableName, template:CompiledTemplate, embedded:Boolean): Result[OpenlawExecutionState] =
    getVariableValue[TemplateDefinition](variableName).flatMap { templateDefinitionOption =>
      templateDefinitionOption.map { templateDefinition =>
        detectCyclicDependency(templateDefinition).map(_ => {
          val newExecution = OpenlawExecutionState(
            id = TemplateExecutionResultId(createAnonymousVariable().name),
            info = info,
            parameters = parameters,
            embedded = embedded,
            sectionLevelStack = if (embedded) this.sectionLevelStack else mutable.Buffer(),
            template = template,
            anonymousVariableCounter = new AtomicInteger(anonymousVariableCounter.get()),
            clock = clock,
            parentExecution = Some(this),
            parentExecutionInternal = Some(this),
            variableRedefinition = template.redefinition,
            templateDefinition = Some(templateDefinition),
            mapping = templateDefinition.mapping,
            remainingElements = mutable.Buffer(template.block.elems: _*),
            executions = this.executions
          )

          val execution = template match {
            case agreement: CompiledAgreement =>
              newExecution.copy(compiledAgreement = Some(agreement))
            case _ =>
              newExecution
          }

          this.subExecutionsInternal.put(variableName, execution)
          execution
        })
      }.getOrElse(Failure(s"template ${variableName.name} was not resolved! ${variables.map(_.name)}"))
    }

  private def detectCyclicDependency(definition: TemplateDefinition): Result[TemplateExecutionResult] =
    detectCyclicDependency(this, definition)

  @tailrec
  private def detectCyclicDependency(execution:TemplateExecutionResult, definition:TemplateDefinition): Result[TemplateExecutionResult] = {
    if(execution.templateDefinition.exists(_ === definition)) {
      Failure(s"cyclic dependency detected on '${definition.name.name}'")
    } else {
      execution.parentExecution match {
        case Some(parent) =>
          detectCyclicDependency(parent, definition)
        case None =>
          Success(execution)
      }
    }
  }

  def structuredMainTemplate(agreement:CompiledAgreement): Result[StructuredAgreement] =
    agreement.structuredMainTemplate(this)

  def structuredInternal(agreement: CompiledAgreement): Result[StructuredAgreement] =
    agreement.structuredInternal(this, templateDefinition.flatMap(_.path))

}

case class StructuredAgreementId(id:String)

object StructuredAgreement {
  implicit val structuredAgreementEnc:Encoder[StructuredAgreement] = deriveEncoder[StructuredAgreement]
  implicit val structuredAgreementDec:Decoder[StructuredAgreement] = deriveDecoder[StructuredAgreement]
}

case class StructuredAgreement(executionResultId:TemplateExecutionResultId, templateDefinition:Option[TemplateDefinition], mainTemplate:Boolean = false, header:TemplateHeader, paragraphs:List[Paragraph] = List(), path:Option[TemplatePath] = None) {
  def title: TemplateTitle = {
    if(header.shouldShowTitle) {
      templateDefinition.map(template => template.name.name).getOrElse(TemplateTitle(""))
    } else {
      TemplateTitle()
    }
  }

  def name: String =
    templateDefinition.map(template => template.path.map(_.path.mkString("/") + "/").getOrElse("") + template.name.name.title).getOrElse("")

  def directory:TemplatePath = path.getOrElse(TemplatePath())
}

sealed trait AgreementElement {
  def serialize:Json
}

object AgreementElement {

  private def className[T](implicit classTag: ClassTag[T]):String = classTag.runtimeClass.getSimpleName

  implicit val agreementElementEnc:Encoder[AgreementElement] = (a: AgreementElement) => {
    Json.obj(
      "name" -> Json.fromString(a.getClass.getSimpleName),
      "value" -> a.serialize
    )
  }
  implicit val agreementElementDec:Decoder[AgreementElement] = (c: HCursor) => {
    for {
      name <- c.downField("name").as[String]
      value <- decodeAgreementElement(name, c.downField("value"))
    } yield value
  }

  private def decodeAgreementElement(name: String, a: ACursor):Decoder.Result[AgreementElement] = {
    name match {
      case _ if name === className[PlainText] =>
        a.as[PlainText]
      case _ if name === className[Title] =>
        a.as[Title]
      case _ if name === className[ImageElement] =>
        a.as[ImageElement]
      case _ if name === className[FreeText] =>
        a.as[FreeText]
      case _ if name === className[Link] =>
        a.as[Link]
      case _ if name === className[VariableElement] =>
        a.as[VariableElement]
      case _ if name === className[SectionElement] =>
        a.as[SectionElement]
      case _ if name === className[ConditionalStart] =>
        a.as[ConditionalStart]
      case _ if name === className[ConditionalEnd] =>
        a.as[ConditionalEnd]
      case _ if name === className[TableElement] =>
        a.as[TableElement]
      case _ if name === className[NoteAnnotation] =>
        a.as[NoteAnnotation]
      case _ if name === className[HeaderAnnotation] =>
        a.as[HeaderAnnotation]
      case _ if name === className[Title] =>
        a.as[Title]
      case _ if name === className[Paragraph] =>
        a.as[Paragraph]
      case _ =>
        Left(DecodingFailure(s"unknown agreement element type $name", List()))
    }
  }
}
/** Special agreement element type used internally by this printer to demark text to be output without any
  * styling or wrapping elements.
  */
final case class PlainText(str: String) extends AgreementElement {
  override def serialize: Json = this.asJson
}
/** Agreement element to wrap a title to support printing titles in downloaded documents.
  */
final case class Title(title: TemplateTitle) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class ImageElement(url: String) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class FreeText(elem: TextElement) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class Link(label:String, url:String) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class VariableElement(name: String, variableType: Option[VariableType], content:List[AgreementElement], dependencies: Seq[String]) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class SectionElement(value: String, lvl:Int, number:Int, resetNumbering:Option[Int], overriddenSymbol: Option[SectionSymbol], overriddenFormat: Option[SectionFormat]) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class ConditionalStart(dependencies: Seq[String]) extends AgreementElement {
  override def serialize: Json = this.asJson
}

final case class ConditionalEnd(dependencies: Seq[String]) extends AgreementElement {
  override def serialize: Json = this.asJson
}

case class ParagraphBuilder(paragraphs:List[Paragraph] = List(), lastParagraph:Paragraph = Paragraph()) {
  def addAllToLastParagraph(elements: List[AgreementElement]): ParagraphBuilder = elements.foldLeft(this)((builder, element) => builder.add(element))

  def add(element: AgreementElement): ParagraphBuilder = this.copy(lastParagraph = lastParagraph.appendElement(element))

  def closeLastParagraph():ParagraphBuilder = this.copy(paragraphs = paragraphs :+ lastParagraph, lastParagraph = Paragraph())

  def build:List[Paragraph] = paragraphs :+ lastParagraph

}

final case class Paragraph(elements: List[AgreementElement] = List()) extends AgreementElement {
  def appendElement(element: AgreementElement): Paragraph = this.copy(elements :+ element)

  override def serialize: Json = this.asJson
}

final case class TableElement(header: List[List[AgreementElement]], rows: List[List[List[AgreementElement]]]) extends AgreementElement {
  val rowCount: Int = rows.size
  val columnCount: Int = header.size

  override def serialize: Json = this.asJson
}

final case class HeaderAnnotation(content:String) extends TemplatePart with AgreementElement {
  override def serialize: Json = this.asJson
}
final case class NoteAnnotation(content:String) extends TemplatePart with AgreementElement {
  override def serialize: Json = this.asJson
}

case class DistinctVariableBuilder(variables:mutable.Buffer[VariableDefinition] = mutable.Buffer(), names:mutable.Set[VariableName] = mutable.Set()) {
  def add(variable: VariableDefinition): DistinctVariableBuilder = {
    variables append variable
    names add variable.name
    this
  }
}

object TemplateExecutionState {
  implicit val templateExecutionStateEq:Eq[TemplateExecutionState] = Eq.fromUniversalEquals
}

sealed trait TemplateExecutionState
case object ExecutionFinished extends TemplateExecutionState
case object ExecutionReady extends TemplateExecutionState
final case class ExecutionFailed(err:Failure[_]) extends TemplateExecutionState
final case class ExecutionWaitForTemplate(variableName:VariableName, template:TemplateSourceIdentifier, willBeUsedForEmbedded:Boolean) extends TemplateExecutionState

case class ActionInfo(action:ActionValue, name:VariableName, executionResult: TemplateExecutionResult)

object TemplateExecutionResultId {
  implicit val templateExecutionResultIdEq:Eq[TemplateExecutionResultId] = Eq.fromUniversalEquals
  implicit val templateExecutionResultIdEnc:Encoder[TemplateExecutionResultId] = deriveEncoder[TemplateExecutionResultId]
  implicit val templateExecutionResultIdDec:Decoder[TemplateExecutionResultId] = deriveDecoder[TemplateExecutionResultId]
  implicit val templateExecutionResultIdKeyEnc:KeyEncoder[TemplateExecutionResultId] = (key: TemplateExecutionResultId) => key.id
  implicit val templateExecutionResultIdKeyDec:KeyDecoder[TemplateExecutionResultId] = (key: String) => Some(TemplateExecutionResultId(key))
}

case class TemplateExecutionResultId(id:String)

trait SignatureProof {
  def validationLink: Link
  def serialize: String
  val fullName:String
  val contractId:ContractId
}

case class ValidationResult(
                             identities:Seq[VariableDefinition],
                             missingInputs:Seq[VariableName],
                             missingIdentities:Seq[VariableName],
                             validationExpressionErrors:Seq[String]) {
  def successful:Boolean = identities.nonEmpty && missingInputs.isEmpty && missingIdentities.isEmpty && validationExpressionErrors.isEmpty
}