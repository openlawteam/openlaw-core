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
import org.adridadou.openlaw.oracles.{ExternalSignatureProof, OpenlawSignatureProof}
import org.adridadou.openlaw.result.{Failure, Result, ResultNel, Success}
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
  def signatureProofs:Map[Email, SignatureProof]
  def parentExecution:Option[TemplateExecutionResult]
  def variables:Seq[VariableDefinition]
  def mapping:Map[VariableName, Expression]
  def aliases:Seq[VariableAliasing]
  def sectionNameMappingInverse:Map[VariableName, String]
  def variableTypes:Seq[VariableType]
  def variableSections:Map[String, Seq[VariableName]]
  def parameters:TemplateParameters
  def executionType:ExecutionType
  def processedSections:Seq[(Section, Int)]
  def executedVariables:Seq[VariableName]
  def agreements:Seq[StructuredAgreement]
  def variableSectionList:Seq[String]
  def executions:Map[ActionIdentifier, Executions]
  def info:OLInformation
  def externalCallStructures: Map[ServiceName, IntegratedServiceDefinition]
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

  def getVariables(varTypes: VariableType*):Seq[(TemplateExecutionResult, VariableDefinition)] =
    getVariables
      .filter(variable => {
        variable.varType(this) match {
          case collectionType:CollectionType =>
            varTypes.contains(collectionType.typeParameter)
          case structuredType:DefinedStructureType =>
            structuredType.structure.typeDefinition.values.exists(s => varTypes.contains(s.varType(this)))
          case domainType:DefinedDomainType =>
            domainType.domain.typeDefinition.values.exists(s => varTypes.contains(s.varType(this)))
          case variableType => varTypes.contains(variableType)
        }}).map((this, _)) ++ subExecutions.values.flatMap(_.getVariables(varTypes:_*))

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
            variable.evaluateT[Identity](result).map(_.toSeq)
          case ExternalSignatureType =>
            variable.evaluateT[ExternalSignature](result)
              .map(_.flatMap(_.identity).toSeq)

          case collectionType: CollectionType if collectionType.typeParameter === IdentityType =>
            variable
              .evaluateT[CollectionValue](result)
              .flatMap { _.map(x => x.list.map(VariableType.convert[Identity]).toList.sequence).getOrElse(Success(Seq())) }

          case collectionType: CollectionType if collectionType.typeParameter === ExternalSignatureType =>
            variable
              .evaluateT[CollectionValue](result)
              .flatMap{ _.map(x => x.list.map(VariableType.convert[ExternalSignature]).toList.sequence).getOrElse(Success(Seq())) }
              .map(_.flatMap(_.identity))

          case structureType: DefinedStructureType if structureType.structure.typeDefinition.values.exists(_.varType(result) === IdentityType) =>
            variable
              .evaluateT[OpenlawMap[VariableName, OpenlawValue]](result)
              .map(_.getOrElse(Map()))
              .flatMap { values =>
                structureType
                  .structure
                  .names
                  .filter(name => structureType.structure.typeDefinition(name).varType(result) === IdentityType)
                  .map(name => VariableType.convert[Identity](values(name)))
                  .toList
                  .sequence
              }

          case structureType: DefinedStructureType if structureType.structure.typeDefinition.values.exists(_.varType(result) === ExternalSignatureType) =>
            variable
              .evaluateT[OpenlawMap[VariableName, OpenlawValue]](result)
              .map(_.getOrElse(Map()))
              .flatMap { values =>
                structureType
                  .structure
                  .names
                  .filter(name => structureType.structure.typeDefinition(name).varType(result) === ExternalSignatureType)
                  .map(name => VariableType.convert[ExternalSignature](values(name)))
                  .toList
                  .sequence
                  .map(_.flatMap(_.identity))
              }

          //TODo - we may need to add this, not sure yet
          /*case domainType: DefinedDomainType if domainType.domain.typeDefinition.values.exists(_.varType(result) === IdentityType) =>
            variable
              .evaluateT[OpenlawMap[VariableName, OpenlawValue]](result)
              .map(_.getOrElse(Map()))
              .flatMap { values =>
                domainType
                  .domain
                  .typeDefinition
                  .filter(name => name.varType(result) === IdentityType)
                  .map(name => VariableType.convert[Identity](values(name)))
                  .toList
                  .sequence
              }*/

          case _ =>
            Success(Seq())
        }
      }
       .toList
       .sequence
       .map(_.flatten)
  }

  def allActions: Result[Seq[ActionInfo]] =
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
            .map(x => x.map(action => ActionInfo(action, executionResult)).toList)
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

  def embedded: Boolean = executionType match {
    case TemplateExecution => false
    case _ => true
  }

  def allProcessedSections:Seq[(Section, Int)] = (embedded, parentExecution)  match {
    case (true, Some(parent)) => parent.allProcessedSections ++ processedSections
    case _ => processedSections
  }

  def getAllExecutedVariables:Seq[(TemplateExecutionResult, VariableName)] =
    executedVariables.distinct.map(name => (this, name)) ++ subExecutions.values.flatMap(_.getAllExecutedVariables)

  @tailrec
  final def getAllVariables:Seq[(TemplateExecutionResult, VariableDefinition)] =
    parentExecution match {
      case Some(execution) =>
        execution.getAllVariables
      case None =>
        getAllVariablesFromRoot
    }

  private def getAllVariablesFromRoot:Seq[(TemplateExecutionResult, VariableDefinition)] =
    getVariables.map(variable => (this, variable)) ++ subExecutions.values.flatMap(_.getAllVariablesFromRoot)

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

  def startEphemeralExecution(name:VariableName, value:OpenlawValue, varType:VariableType): Result[TemplateExecutionResult] =
    this.getAliasOrVariableType(name) match {
      case Success(_) =>
        Failure(s"${name.name} has already been defined!")
      case Failure(_,_) =>
        varType.internalFormat(value).flatMap { internalFormat =>
          val result = OpenlawExecutionState(
            id = TemplateExecutionResultId(UUID.randomUUID().toString),
            info = info,
            executionType = BlockExecution,
            parameters = TemplateParameters(name.name -> internalFormat),
            sectionLevelStack = mutable.Buffer(),
            template = CompiledAgreement(header = TemplateHeader()),
            clock = clock,
            parentExecution = Some(this),
            executions = this.executions,
            variableRedefinition = VariableRedefinition()
          )

					result.registerNewType(varType).map(newResult => {
						newResult.variablesInternal.append(VariableDefinition(name, Some(VariableTypeDefinition(name = varType.name, None))))
						newResult.executedVariablesInternal.append(name)
						newResult
					})
        }
    }
}

object SerializableTemplateExecutionResult {
  implicit val serializableTemplateExecutionResultEnc:Encoder[SerializableTemplateExecutionResult] = deriveEncoder
  implicit val serializableTemplateExecutionResultDec:Decoder[SerializableTemplateExecutionResult] = deriveDecoder
  implicit val serializableTemplateExecutionResultEq:Eq[SerializableTemplateExecutionResult] = Eq.fromUniversalEquals

  implicit val clockEnc:Encoder[Clock] = (a: Clock) => Json.fromString(a.getZone.getId)
  implicit val clockDec:Decoder[Clock] = (c: HCursor) => c.as[String].map(id => Clock.system(ZoneId.of(id)))
  implicit val clockDecEq:Eq[Clock] = Eq.fromUniversalEquals
}

final case class SerializableTemplateExecutionResult(id:TemplateExecutionResultId,
                                               info:OLInformation,
                                               templateDefinition: Option[TemplateDefinition] = None,
                                               subExecutionIds:Map[VariableName, TemplateExecutionResultId],
                                               templateExecutions:Map[TemplateExecutionResultId, SerializableTemplateExecutionResult],
                                               executions:Map[ActionIdentifier, Executions],
                                               parentExecutionId:Option[TemplateExecutionResultId],
                                               agreements:Seq[StructuredAgreement],
                                               variableSectionList:Seq[String],
                                               signatureProofs:Map[Email, SignatureProof],
                                               variables:Seq[VariableDefinition],
                                               executedVariables:Seq[VariableName],
                                               mapping:Map[VariableName, Expression],
                                               aliases:Seq[VariableAliasing],
                                               sectionNameMappingInverse:Map[VariableName, String],
                                               variableTypes:Seq[VariableType],
                                               variableSections:Map[String, Seq[VariableName]],
                                               parameters:TemplateParameters,
                                               executionType:ExecutionType,
                                               processedSections:Seq[(Section, Int)],
                                               externalCallStructures:Map[ServiceName, IntegratedServiceDefinition],
                                               clock:Clock) extends TemplateExecutionResult {

  override def subExecutions: Map[VariableName, TemplateExecutionResult] = subExecutionIds.flatMap({case (identifier, executionId) => templateExecutions.get(executionId).map(identifier -> _)})
  override def parentExecution: Option[TemplateExecutionResult] = parentExecutionId.flatMap(templateExecutions.get)
}

object ExecutionType {
  implicit val executionTypeEq:Eq[ExecutionType] = Eq.fromUniversalEquals
}

sealed trait ExecutionType
case object TemplateExecution extends ExecutionType
case object ClauseExecution extends ExecutionType
case object BlockExecution extends ExecutionType

final case class OpenlawExecutionState(
                                    id:TemplateExecutionResultId,
                                    parameters:TemplateParameters,
                                    executionType:ExecutionType,
                                    info:OLInformation,
                                    executions:Map[ActionIdentifier,Executions],
                                    signatureProofs:Map[Email, SignatureProof] = Map(),
                                    template:CompiledTemplate,
                                    anonymousVariableCounter:AtomicInteger = new AtomicInteger(0),
                                    processedAnonymousVariableCounter:AtomicInteger = new AtomicInteger(0),
                                    variablesInternal:mutable.Buffer[VariableDefinition] = mutable.Buffer(),
                                    aliasesInternal:mutable.Buffer[VariableAliasing] = mutable.Buffer(),
                                    executedVariablesInternal:mutable.Buffer[VariableName] = mutable.Buffer(),
                                    variableSectionsInternal:mutable.Map[String, mutable.Buffer[VariableName]] = mutable.Map(),
                                    variableSectionListInternal:mutable.Buffer[String] = mutable.Buffer(),
                                    agreementsInternal:mutable.Buffer[StructuredAgreement] = mutable.Buffer(),
                                    subExecutionsInternal:mutable.Map[VariableName, OpenlawExecutionState] = mutable.Map(),
                                    forEachExecutions:mutable.Buffer[TemplateExecutionResultId] = mutable.Buffer(),
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
                                    externalCallStructures: Map[ServiceName, IntegratedServiceDefinition] = Map(),
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
        case Some(parent) =>
					parent.addLastSectionByLevel(lvl, sectionValue)
        case None =>
					lastSectionByLevel put (lvl , sectionValue)
      }
    } else {
      lastSectionByLevel put (lvl , sectionValue)
    }
  }

  override def subExecutions: Map[VariableName, TemplateExecutionResult] = subExecutionsInternal.toMap

  @scala.annotation.tailrec
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

	@scala.annotation.tailrec
  def addProcessedSection(section: Section, number: Int):Unit = (embedded, parentExecutionInternal) match {
    case (true, Some(parent)) => parent.addProcessedSection(section, number)
    case _ => processedSectionsInternal append (section -> number)
  }

	@scala.annotation.tailrec
  def addSectionLevelStack(newSectionValues: Seq[Int]):Unit =
    if(embedded) {
      parentExecutionInternal match {
        case Some(parent) => parent.addSectionLevelStack(newSectionValues)
        case None => sectionLevelStack appendAll newSectionValues
      }
    } else {
      sectionLevelStack appendAll newSectionValues
    }

  def allSectionLevelStack:Seq[Int] =
    if(embedded) {
      parentExecutionInternal match {
        case Some(parent) => parent.allSectionLevelStack ++ sectionLevelStack
        case None => sectionLevelStack
      }
    } else {
      sectionLevelStack
    }

  def executionLevel:Int = executionLevel(parentExecution, 0)

  def validateExecution: Result[ValidationResult] = {
    val variables = getAllExecutedVariables
      .flatMap({case (result, name) => result.getVariable(name).map(variable => (result, variable))})
      .filter({case (result, variable) => variable.varType(result) match {
        case _:NoShowInForm => false
        case _ => true
      }})

    val identities = variables.filter({ case (result, variable) =>
      variable.varType(result) match {
        case IdentityType => true
        case ExternalSignatureType => true
        case collectionType:CollectionType if IdentityType.identityTypes.contains(collectionType.typeParameter) => true
        case structureType:DefinedStructureType if structureType.structure.typeDefinition.values.exists(s => IdentityType.identityTypes.contains(s.varType(this))) => true
        case domainType:DefinedDomainType if domainType.domain.typeDefinition.values.exists(s => IdentityType.identityTypes.contains(s.varType(this))) => true
        case _ => false
      }
    }).map({case (_, variable) => variable})

    val missingIdentitiesResult = {
      variables.map { case (result, variable) =>
        variable.varType(result) match {
          case IdentityType =>
            Success(resultFromMissingInput(variable.missingInput(result)))
          case ExternalSignatureType =>
            Success(resultFromMissingInput(variable.missingInput(result)))

          case collectionType: CollectionType if IdentityType.identityTypes.contains(collectionType.typeParameter) =>
            result.getVariableValue[CollectionValue](variable.name).map {
              case Some(value) if value.size =!= value.values.size =>
                (Seq(variable.name), Seq())
              case Some(_) =>
                (Seq(), Seq())
              case None =>
                (Seq(variable.name), Seq())
            }

          case structureType: DefinedStructureType if structureType.structure.typeDefinition.values.exists(s => IdentityType.identityTypes.contains(s.varType(this))) =>
            result.getVariableValue[OpenlawMap[VariableName, OpenlawValue]](variable.name).map { values =>
              val identityProperties = structureType.structure.typeDefinition
                .filter({ case (_, propertyType) => IdentityType.identityTypes.contains(propertyType.varType(this))})
                .map({ case (propertyName, _) => propertyName }).toSeq

              if (identityProperties.forall(values.getOrElse(Map()).contains)) {
                (Seq(), Seq())
              } else {
                (Seq(variable.name), Seq())
              }
            }

          case domainType: DefinedDomainType if domainType.domain.typeDefinition.values.exists(s => IdentityType.identityTypes.contains(s.varType(this))) =>
            result.getVariableValue[OpenlawMap[VariableName, OpenlawValue]](variable.name).map { values =>
              val identityProperties = domainType.domain.typeDefinition
                .filter({ case (_, propertyType) => IdentityType.identityTypes.contains(propertyType.varType(this))})
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
      executionType = executionType,
      processedSections = processedSections,
      externalCallStructures = externalCallStructures,
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
    val missingInputs = getAllExecutedVariables
			.map({case (result, variable) => variable.missingInput(result)})

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
    val currentCounter = anonymousVariableCounter.incrementAndGet()
    VariableName(generateAnonymousName(currentCounter))
  }

  def generateAnonymousName(counter:Int):String = s"@@anonymous_$counter@@"

  def getTemplateIdentifier:Option[TemplateSourceIdentifier] = state match {
    case ExecutionWaitForTemplate(_, identifier, _) =>
      Some(identifier)
    case _ => None
  }

  def getCompiledTemplate(templates: Map[TemplateSourceIdentifier, CompiledTemplate]):Option[CompiledTemplate] =
    getTemplateIdentifier flatMap templates.get

  def startForEachExecution(variableName:VariableName, template:CompiledTemplate, name:VariableName, value:OpenlawValue, varType:VariableType): Result[OpenlawExecutionState] =
    for {
      internalFormat <- varType.internalFormat(value)
      result <- startSubExecution(variableName, template, executionType = BlockExecution, Map(name -> internalFormat))
    } yield {
      this.forEachExecutions append result.id
      result
    }

  private def getSectionLevelStack(executionType:ExecutionType):mutable.Buffer[Int] = executionType match {
    case TemplateExecution => mutable.Buffer()
    case _ => this.sectionLevelStack
  }

  def startSubExecution(variableName:VariableName, template:CompiledTemplate, executionType:ExecutionType, overrideParameters:Map[VariableName, String] = Map()): Result[OpenlawExecutionState] =
    getVariableValue[TemplateDefinition](variableName).flatMap { templateDefinitionOption =>
      templateDefinitionOption.map { templateDefinition =>
        detectCyclicDependency(templateDefinition).map(_ => {
          val newExecution = OpenlawExecutionState(
            id = TemplateExecutionResultId(createAnonymousVariable().name),
            info = info,
            parameters = parameters ++ overrideParameters,
            executionType = executionType,
            sectionLevelStack = getSectionLevelStack(executionType),
            template = template,
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

  def findExecutionResultInternal(executionResultId: TemplateExecutionResultId): Option[OpenlawExecutionState] = {
    if(id === executionResultId) {
      Some(this)
    } else {
      subExecutionsInternal.values.flatMap(_.findExecutionResultInternal(executionResultId)).headOption
    }
  }

}

final case class StructuredAgreementId(id:String)

object StructuredAgreement {
  implicit val structuredAgreementEnc:Encoder[StructuredAgreement] = deriveEncoder
  implicit val structuredAgreementDec:Decoder[StructuredAgreement] = deriveDecoder
}

final case class StructuredAgreement(executionResultId:TemplateExecutionResultId, templateDefinition:Option[TemplateDefinition], mainTemplate:Boolean = false, header:TemplateHeader, paragraphs:List[Paragraph] = List(), path:Option[TemplatePath] = None) {
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
final case class VariableElement(name: VariableName, variableType: Option[VariableType], content:List[AgreementElement], dependencies: Seq[String]) extends AgreementElement {
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

final case class ParagraphBuilder(paragraphs:List[Paragraph] = List(), lastParagraph:Paragraph = Paragraph()) {
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

final case class DistinctVariableBuilder(variables:mutable.Buffer[VariableDefinition] = mutable.Buffer(), names:mutable.Set[VariableName] = mutable.Set()) {
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
final case class ExecutionWaitForTemplate(variableName:VariableName, template:TemplateSourceIdentifier, executionType:ExecutionType) extends TemplateExecutionState

final case class ActionInfo(action:ActionValue, executionResult: TemplateExecutionResult) {
  def identifier:Result[ActionIdentifier] = action.identifier(executionResult)
}

object TemplateExecutionResultId {
  implicit val templateExecutionResultIdEq:Eq[TemplateExecutionResultId] = Eq.fromUniversalEquals
  implicit val templateExecutionResultIdEnc:Encoder[TemplateExecutionResultId] = deriveEncoder
  implicit val templateExecutionResultIdDec:Decoder[TemplateExecutionResultId] = deriveDecoder
  implicit val templateExecutionResultIdKeyEnc:KeyEncoder[TemplateExecutionResultId] = (key: TemplateExecutionResultId) => key.id
  implicit val templateExecutionResultIdKeyDec:KeyDecoder[TemplateExecutionResultId] = (key: String) => Some(TemplateExecutionResultId(key))
}

final case class TemplateExecutionResultId(id:String)

object SignatureProof {

  def className[T]()(implicit cls:ClassTag[T]):String = cls.runtimeClass.getName

  implicit val signatureProofEnc:Encoder[SignatureProof] = (a: SignatureProof) => Json.obj(
    "type" -> Json.fromString(a.getClass.getName),
    "value" -> a.serialize
  )
  implicit val signatureProofDec:Decoder[SignatureProof] = (c: HCursor) => {
    c.downField("type").as[String].flatMap(classType => {
      if (classType === className[OpenlawSignatureProof]) {
        c.downField("value").as[OpenlawSignatureProof]
      } else if (classType === className[ExternalSignatureProof]) {
        c.downField("value").as[ExternalSignatureProof]
      } else {
        Left(DecodingFailure(s"unknown signature proof type $classType", List()))
      }
    })
  }
}

trait SignatureProof {
  def validationLink: Link
  def serialize: Json
  val fullName:String
  val contractId:ContractId
}

final case class ValidationResult(
                             identities:Seq[VariableDefinition],
                             missingInputs:Seq[VariableName],
                             missingIdentities:Seq[VariableName],
                             validationExpressionErrors:Seq[String]) {
  def successful:Boolean = identities.nonEmpty && missingInputs.isEmpty && missingIdentities.isEmpty && validationExpressionErrors.isEmpty
}

object ActionIdentifier {
  implicit val actionIdentifierEnc:Encoder[ActionIdentifier] = deriveEncoder
  implicit val actionIdentifierDec:Decoder[ActionIdentifier] = deriveDecoder
  implicit val actionIdentifierKeyEnc:KeyEncoder[ActionIdentifier] = (key: ActionIdentifier) => key.identifier
  implicit val actionIdentifierKeyDec:KeyDecoder[ActionIdentifier] = (key: String) => Some(ActionIdentifier(key))
  implicit val actionIdentifierEq:Eq[ActionIdentifier] = Eq.fromUniversalEquals
}

final case class ActionIdentifier(identifier:String)
