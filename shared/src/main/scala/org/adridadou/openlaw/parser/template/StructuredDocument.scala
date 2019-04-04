package org.adridadou.openlaw.parser.template
import java.time.Clock
import java.util.concurrent.atomic.AtomicInteger

import cats.Eq
import org.adridadou.openlaw.values.{ContractId, TemplateParameters, TemplateTitle}

import scala.collection.mutable
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import VariableName._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import org.adridadou.openlaw.result.{Failure, Result, Success}

case class TemplateExecutionResult(
                                    id:TemplateExecutionResultId,
                                    parameters:TemplateParameters,
                                    embedded:Boolean,
                                    signatureProofs:Map[Email, SignatureProof] = Map(),
                                    template:CompiledTemplate,
                                    forEachQueue:mutable.Buffer[Any] = mutable.Buffer(),
                                    anonymousVariableCounter:AtomicInteger, variables:mutable.Buffer[VariableDefinition] = mutable.Buffer(),
                                    aliases:mutable.Buffer[VariableAliasing] = mutable.Buffer(),
                                    executedVariables:mutable.Buffer[VariableName] = mutable.Buffer(),
                                    variableSectionsInternal:mutable.Map[String, mutable.Buffer[VariableName]] = mutable.Map(),
                                    variableSectionList:mutable.Buffer[String] = mutable.Buffer(),
                                    agreements:mutable.Buffer[StructuredAgreement] = mutable.Buffer(),
                                    subExecutions:mutable.Map[VariableName, TemplateExecutionResult] = mutable.Map(),
                                    forEachExecutions:mutable.Buffer[TemplateExecutionResult] = mutable.Buffer(),
                                    finishedEmbeddedExecutions:mutable.Buffer[TemplateExecutionResult] = mutable.Buffer(),
                                    state:TemplateExecutionState = ExecutionReady,
                                    remainingElements:mutable.Buffer[TemplatePart] = mutable.Buffer(),
                                    parentExecution:Option[TemplateExecutionResult] = None,
                                    compiledAgreement:Option[CompiledAgreement] = None,
                                    variableRedefinition: VariableRedefinition,
                                    templateDefinition: Option[TemplateDefinition] = None,
                                    mapping:Map[VariableName, Expression] = Map(),
                                    variableTypes: mutable.Buffer[VariableType] = mutable.Buffer(VariableType.allTypes() : _*),
                                    sectionLevelStack: mutable.Buffer[Int] = mutable.Buffer(),
                                    sectionNameMapping: mutable.Map[String, VariableName] = mutable.Map(),
                                    sectionNameMappingInverse: mutable.Map[VariableName, String] = mutable.Map(),
                                    processedSections: mutable.Buffer[(Section, Int)] = mutable.Buffer(),
                                    lastSectionByLevel:mutable.Map[Int, String] = mutable.Map(),
                                    clock:Clock) {
  def addLastSectionByLevel(lvl: Int, sectionValue: String):Unit = {
    if(embedded) {
      parentExecution match {
        case Some(parent) => parent.addLastSectionByLevel(lvl, sectionValue)
        case None => lastSectionByLevel put (lvl , sectionValue)
      }
    } else {
      lastSectionByLevel put (lvl , sectionValue)
    }
  }

  def getLastSectionByLevel(idx: Int): String = {
    if(embedded) {
      parentExecution match {
        case Some(parent) => parent.getLastSectionByLevel(idx)
        case None => lastSectionByLevel.getOrElse(idx,"")
      }
    } else {
      lastSectionByLevel.getOrElse(idx,"")
    }
  }

  def addProcessedSection(section: Section, number: Int):Unit = (embedded, parentExecution) match {
    case (true, Some(parent)) => parent.addProcessedSection(section, number)
    case _ => processedSections append (section -> number)
  }

  def allProcessedSections:mutable.Buffer[(Section, Int)] = (embedded, parentExecution)  match {
    case (true, Some(parent)) => parent.allProcessedSections ++ processedSections
    case _ => processedSections
  }

  def addSectionLevelStack(newSectionValues: Seq[Int]):Unit = {
    if(embedded) {
      parentExecution match {
        case Some(parent) => parent.addSectionLevelStack(newSectionValues)
        case None => sectionLevelStack appendAll newSectionValues
      }
    } else {
      sectionLevelStack appendAll newSectionValues
    }
  }


  def allSectionLevelStack:mutable.Buffer[Int] = {
    if(embedded) {
      parentExecution match {
        case Some(parent) => parent.allSectionLevelStack ++ sectionLevelStack
        case None => sectionLevelStack
      }
    } else {
      sectionLevelStack
    }
  }

  def getSignatureProof(identity: Identity):Option[SignatureProof] = signatureProofs.get(identity.email) match {
    case Some(value) => Some(value)
    case None => parentExecution.flatMap(_.getSignatureProof(identity))
  }

  def hasSigned(email: Email):Boolean =
    if(signatureProofs.contains(email)) true else parentExecution.exists(_.hasSigned(email))

  def findExecutionResult(executionResultId: TemplateExecutionResultId): Option[TemplateExecutionResult] = {
    if(id === executionResultId) {
      Some(this)
    } else {
      subExecutions.values.flatMap(_.findExecutionResult(executionResultId)).headOption
    }
  }

  def getParameter(name: String):Option[String] = getParameter(VariableName(name))

  def getParameter(name: VariableName):Option[String] = parameters.get(name) match {
    case Some(value) =>
      Some(value)
    case None =>
      parentExecution.flatMap(_.getParameter(name))
  }

  def executionLevel:Int = executionLevel(parentExecution, 0)

  def validateExecution:ValidationResult = {
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

    val missingIdentitiesResult = variables.map({ case (result, variable) =>
      variable.varType(result) match {
        case IdentityType =>
          resultFromMissingInput(variable.missingInput(result))
        case collectionType:CollectionType if collectionType.typeParameter === IdentityType =>
          result.getVariableValue[CollectionValue](variable.name) match {
            case Some(value) if value.size =!= value.values.size =>
              (Seq(variable.name), Seq())
            case Some(_) =>
              (Seq(), Seq())
            case None =>
              (Seq(variable.name), Seq())
          }

        case structureType:DefinedStructureType if structureType.structure.typeDefinition.values.exists(_ === IdentityType) =>
          val values = result.getVariableValue[Map[VariableName, Any]](variable.name)
          val identityProperties = structureType.structure.typeDefinition
            .filter({case (_,propertyType) => propertyType === IdentityType})
            .map({case (propertyName,_) => propertyName}).toSeq

          if(identityProperties.forall(values.getOrElse(Map()).contains)) {
            (Seq(), Seq())
          } else {
            (Seq(variable.name), Seq())
          }

        case _ =>
          (Seq(), Seq())
      }
    })

    val identitiesErrors = missingIdentitiesResult.flatMap({
      case (_, errors) =>  errors
    })

    val missingIdentities = missingIdentitiesResult.flatMap({
      case (values, _) =>  values
    })

    val (missingInputs, additionalErrors) = resultFromMissingInput(allMissingInput)

    ValidationResult(
      identities = identities,
      missingInputs = missingInputs,
      missingIdentities = missingIdentities,
      validationExpressionErrors = validate() ++ additionalErrors ++ identitiesErrors
    )
  }

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

  def registerNewType(variableType: VariableType): Result[TemplateExecutionResult] = {
    findVariableType(VariableTypeDefinition(variableType.name)) match {
      case None =>
        variableTypes append variableType
        Success(this)
      case Some(_) =>
        Failure(s"the variable type ${variableType.name} as already been defined")
    }
  }

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

  def getTemplateDefinitionForVariable(name: VariableName):Option[TemplateDefinition] =
    variables.find(_.name === name) match {
      case Some(_) =>
        templateDefinition
      case None =>
        parentExecution.flatMap(_.getTemplateDefinitionForVariable(name))
    }

  def notFinished: Boolean = state match {
    case ExecutionFinished => false
    case _ => true
  }

  def allActions():Seq[ActionInfo] = {
    getAllExecutedVariables.flatMap({
          case (executionResult, variableName) =>
            executionResult.getVariable(variableName)
              .flatMap(variable => variable.varType(executionResult) match {
                case actionType: ActionType =>
                  variable.evaluate(executionResult).map(actionType.actionValue)
                case _ =>
                  None
              }).map(action => ActionInfo(action, variableName, executionResult))
        })
  }

  def allIdentityEmails:Seq[Email] = allIdentities().map(_.email).distinct

  def allIdentities():Seq[Identity] = {
    getAllExecutedVariables
      .flatMap({case (result, name) => result.getVariable(name).map(variable => (result, variable))})
      .flatMap({ case (result, variable) =>
        variable.varType(result) match {
          case IdentityType =>
            variable.evaluate(result).map(VariableType.convert[Identity]).toSeq
          case collectionType:CollectionType if collectionType.typeParameter === IdentityType =>
            variable.evaluate(result)
              .map(col => VariableType.convert[CollectionValue](col).list.map(VariableType.convert[Identity])).getOrElse(Seq())
          case structureType:DefinedStructureType if structureType.structure.typeDefinition.values.exists(_ === IdentityType) =>
            val values = variable.evaluate(result).map(VariableType.convert[Map[VariableName, Any]]).getOrElse(Map())

            structureType.structure.names
              .filter(name => structureType.structure.typeDefinition(name) === IdentityType)
                .map(name => VariableType.convert[Identity](values(name)))

          case _ =>
            Seq()
        }
      })
  }

  def createAnonymousVariable():VariableName = {
    this.parentExecution.map(_.createAnonymousVariable()) match {
      case Some(name) => name
      case None =>
        val currentCounter = anonymousVariableCounter.incrementAndGet()
        VariableName(s"@@anonymous_$currentCounter@@")
    }
  }

  def sections:Map[String, Seq[VariableName]] = variableSectionsInternal
    .map({
      case (name, sectionVariables) => name -> sectionVariables.distinct
    }).toMap

  def getExpression(name: VariableName):Option[Expression] = getAlias(name) match {
    case Some(alias) =>
      Some(alias)
    case None =>
      getVariable(name)
  }

  def getExecutedVariables:Seq[VariableName] = {
    val variableNames = getAllVariableNames
    getAllExecutedVariables.
      filter({case (_, variable) => variableNames.contains(variable)})
      .map({case (_, name) => name}).distinct
  }

  def getAllExecutionResults:Seq[TemplateExecutionResult] = {
    subExecutions.values.flatMap(_.getAllExecutionResults).toSeq ++ Seq(this)
  }

  def getAllExecutedVariables:Seq[(TemplateExecutionResult, VariableName)] = {
    executedVariables.distinct.map(name => (this, name)) ++ subExecutions.values.flatMap(_.getAllExecutedVariables)
  }

  def getTemplateIdentifier:Option[TemplateSourceIdentifier] = state match {
    case ExecutionWaitForTemplate(_, identifier, _) =>
      Some(identifier)
    case _ => None
  }

  def getCompiledTemplate(templates: Map[TemplateSourceIdentifier, CompiledTemplate]):Option[CompiledTemplate] =
    getTemplateIdentifier flatMap templates.get

  def startForEachExecution(variableName:VariableName, template:CompiledTemplate, name:VariableName, value:Any, varType:VariableType): Result[TemplateExecutionResult] = {
    startSubExecution(variableName, template, embedded = true).map(result => {
      val newResult = result.copy(parameters = parameters + (name -> varType.internalFormat(value)))
      this.forEachExecutions append newResult
      newResult
    })
  }

  def startClauseExecution(variableName:VariableName, template:CompiledTemplate): Result[TemplateExecutionResult] =
    startSubExecution(variableName, template, embedded = true).map(result => {
      result
    })

  def startTemplateExecution(variableName:VariableName, template:CompiledTemplate): Result[TemplateExecutionResult] =
    startSubExecution(variableName, template, embedded = false)

  private def startSubExecution(variableName:VariableName, template:CompiledTemplate, embedded:Boolean): Result[TemplateExecutionResult] = {
    getVariableValue[TemplateDefinition](variableName).map(templateDefinition => {
      detectCyclicDependency(templateDefinition).map(_ => {
        val newExecution = TemplateExecutionResult(
          id = TemplateExecutionResultId(createAnonymousVariable().name),
          parameters = parameters,
          embedded = embedded,
          sectionLevelStack = if (embedded) this.sectionLevelStack else mutable.Buffer(),
          template = template,
          anonymousVariableCounter = new AtomicInteger(anonymousVariableCounter.get()),
          clock = clock,
          parentExecution = Some(this),
          variableRedefinition = template.redefinition,
          templateDefinition = Some(templateDefinition),
          mapping = templateDefinition.mapping,
          remainingElements = mutable.Buffer(template.block.elems: _*)
        )

        val execution = template match {
          case agreement: CompiledAgreement =>
            newExecution.copy(compiledAgreement = Some(agreement))
          case _ =>
            newExecution
        }

        this.subExecutions.put(variableName, execution)

        execution
      })
    }).getOrElse(Failure(s"template ${variableName.name} was not resolved! ${variables.map(_.name)}"))
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

  def structuredMainTemplate(agreement:CompiledAgreement): StructuredAgreement =
    agreement.structuredMainTemplate(this)

  def structuredInternal(agreement: CompiledAgreement): StructuredAgreement =
    agreement.structuredInternal(this, templateDefinition.flatMap(_.path))

  def getVariable(variable:VariableDefinition):Option[VariableDefinition] =
    getVariable(variable.name)

  def getVariable(name:VariableName):Option[VariableDefinition] = {
    if(this.sectionNameMappingInverse.contains(name)) {
      this.variables.find(definition => definition.name === name && definition.varType(this) === SectionType)
    }else {
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

  def getAliasOrVariableType(name:VariableName): Result[VariableType] =
    getExpression(name).map(_.expressionType(this))
      .map(varType => Success(varType))
      .getOrElse(Failure(s"${name.name} cannot be resolved!"))

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

  def getVariableValues[T](varType: VariableType)(implicit classTag:ClassTag[T]):Seq[T] = getVariables(varType)
      .flatMap({case (execution, variable) => variable.evaluate(execution).map(getVariableValue[T](_, variable.varType(this)))})

  def getVariableValue[T](name: VariableName)(implicit classTag:ClassTag[T]):Option[T] =
    getVariable(name)
      .flatMap(variable => variable.evaluate(this).map(getVariableValue[T](_, variable.varType(this))))

  private def getVariableValue[T](value:Any, variableType:VariableType)(implicit classTag: ClassTag[T]):T = VariableType.convert[T](value)

  def validate(): Seq[String] = getVariableValues[Validation](ValidationType)
    .flatMap(_.validate(this).left.toOption.map(_.message))
}

case class StructuredAgreementId(id:String)

case class StructuredAgreement(executionResult: TemplateExecutionResult, mainTemplate:Boolean = false, header:TemplateHeader, paragraphs:List[Paragraph] = List(), path:Option[TemplatePath] = None) {
  def title: TemplateTitle = {
    if(header.shouldShowTitle) {
      executionResult.templateDefinition.map(template => template.name.name).getOrElse(TemplateTitle(""))
    } else {
      TemplateTitle("")
    }
  }

  def name: String =
    executionResult.templateDefinition.map(template => template.path.map(_.path.mkString("/") + "/").getOrElse("") + template.name.name.title).getOrElse("")

  def directory:TemplatePath = path.getOrElse(TemplatePath())
}

trait AgreementElement

case class ImageElement(url: String) extends AgreementElement
case class FreeText(elem: TextElement) extends AgreementElement
case class Link(label:String, url:String) extends AgreementElement
case class VariableElement(name: String, variableType: Option[VariableType], content:List[AgreementElement], dependencies: Seq[String]) extends AgreementElement
case class SectionElement(value: String, lvl:Int, number:Int, resetNumbering:Option[Int], overriddenSymbol: Option[SectionSymbol], overridenFormat: Option[SectionFormat]) extends AgreementElement
case class ConditionalStart(dependencies: Seq[String]) extends AgreementElement
case class ConditionalStartWithElse(dependencies: Seq[String]) extends AgreementElement
case class ConditionalEnd(dependencies: Seq[String]) extends AgreementElement
case class ConditionalEndWithElse(dependencies: Seq[String]) extends AgreementElement

case class ClauseAnchorElement(variableName:VariableName) extends AgreementElement

case class ParagraphBuilder(paragraphs:List[Paragraph] = List(), lastParagraph:Paragraph = Paragraph()) {
  def addAllToLastParagraph(elements: List[AgreementElement]): ParagraphBuilder = elements.foldLeft(this)((builder, element) => builder.add(element))

  def add(element: AgreementElement): ParagraphBuilder = this.copy(lastParagraph = lastParagraph.appendElement(element))

  def closeLastParagraph():ParagraphBuilder = this.copy(paragraphs = paragraphs :+ lastParagraph, lastParagraph = Paragraph())

  def build:List[Paragraph] = paragraphs :+ lastParagraph

}

case class Paragraph(elements: List[AgreementElement] = List()) extends AgreementElement {
  def appendElement(element: AgreementElement): Paragraph = this.copy(elements :+ element)
}

case class TableElement(header: List[List[AgreementElement]], rows: List[List[List[AgreementElement]]]) extends AgreementElement {
  val rowCount: Int = rows.size
  val columnCount: Int = header.size
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
final case class ExecutionWaitForTemplate(variableName:VariableName, template:TemplateSourceIdentifier, willBeUsedForEmbedded:Boolean) extends TemplateExecutionState

case class ActionInfo(action:ActionValue, name:VariableName, executionResult: TemplateExecutionResult)

object TemplateExecutionResultId {
  implicit val templateExecutionResultIdEq:Eq[TemplateExecutionResultId] = Eq.fromUniversalEquals
  implicit val templateExecutionResultIdEnc:Encoder[TemplateExecutionResultId] = deriveEncoder[TemplateExecutionResultId]
  implicit val templateExecutionResultIdDec:Decoder[TemplateExecutionResultId] = deriveDecoder[TemplateExecutionResultId]
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