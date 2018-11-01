package org.adridadou.openlaw.client

import java.time.Clock

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._

import scala.scalajs.js
import cats.implicits._
import org.adridadou.openlaw.oracles.{OpenlawSignatureProof, UserId}
import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template.printers.ReviewHtmlAgreementPrinter
import org.adridadou.openlaw.values.{TemplateParameters, TemplateTitle}
import org.adridadou.openlaw.vm.OpenlawExecutionEngine

import scala.scalajs.js.Dictionary
import scala.scalajs.js.JSConverters._

/**
  * Created by davidroon on 05.05.17.
  */
@JSExportTopLevel("Openlaw")
object Openlaw {

  val clock: Clock = Clock.systemDefaultZone()
  val engine = new OpenlawExecutionEngine
  val markdown = new OpenlawTemplateLanguageParserService(Clock.systemUTC())

  @JSExport
  def compileTemplate(text:String) : js.Dictionary[Any] = markdown.compileTemplate(text, clock) match {
    case Left(err) => js.Dictionary(
      "isError" -> true,
      "errorMessage" -> err,
      "compiledTemplate" -> js.undefined
    )
    case Right(result) => js.Dictionary(
      "isError" -> false,
      "errorMessage" -> "",
      "compiledTemplate" -> result
    )
  }

  @JSExport
  def execute(compiledTemplate:CompiledTemplate, jsTemplates:js.Dictionary[CompiledTemplate], jsParams:js.Dictionary[Any]) : js.Dictionary[Any] = {
    val templates = jsTemplates.map({ case (name, template) => TemplateSourceIdentifier(TemplateTitle(name)) -> template}).toMap
    val executionResult = engine.execute(compiledTemplate, prepareParameters(jsParams), templates)
    handleExecutionResult(executionResult)
  }

  @JSExport
  def executeForReview(compiledTemplate:CompiledTemplate, proofs:js.Dictionary[String], jsTemplates:js.Dictionary[CompiledTemplate], jsParams:js.Dictionary[Any]) : js.Dictionary[Any] = {
    val templates = jsTemplates.map({ case (name, template) => TemplateSourceIdentifier(TemplateTitle(name)) -> template}).toMap
    val executionResult = engine.execute(
      compiledTemplate,
      prepareParameters(jsParams),
      templates,
      proofs.flatMap({ case (userId, proof) => OpenlawSignatureProof.deserialize(proof).map(UserId(userId) -> _).toOption}).toMap
    )
    handleExecutionResult(executionResult)
  }

  @JSExport
  def resumeExecution(executionResult:TemplateExecutionResult, jsTemplates:js.Dictionary[CompiledTemplate]) : js.Dictionary[Any] = {
    val templates = jsTemplates.map({ case (name, template) => TemplateSourceIdentifier(TemplateTitle(name)) -> template}).toMap
    handleExecutionResult(engine.resumeExecution(executionResult, templates))
  }

  @JSExport
  def createAddress(address:js.Dictionary[String]):String = {
    AddressType.internalFormat(Address(
      formattedAddress = printAddressElement(address.get("address")),
      placeId = printAddressElement(address.get("placeId")),
      streetName = printAddressElement(address.get("streetName")),
      streetNumber = printAddressElement(address.get("streetNumber")),
      city = printAddressElement(address.get("city")),
      state = printAddressElement(address.get("state")),
      country = printAddressElement(address.get("country")),
      zipCode = printAddressElement(address.get("zipCode"))
    ))
  }

  private def printAddressElement(optStr:Option[String]):String = optStr.getOrElse("n/a")

  @JSExport
  def validationErrors(result:ValidationResult):js.Array[String] = result.validationExpressionErrors.toJSArray

  @JSExport
  def validateContract(executionResult:TemplateExecutionResult):ValidationResult = {
    val variables = executionResult.getAllExecutedVariables
      .flatMap({case (result, name) => result.getVariable(name).map(variable => (result, variable))})
      .filter({case (_, variable) => variable.varType(executionResult) match {
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

    val (missingInputs, additionalErrors) = resultFromMissingInput(executionResult.allMissingInput)

    ValidationResult(
      identities = identities,
      missingInputs = missingInputs,
      missingIdentities = missingIdentities,
      validationExpressionErrors = executionResult.validate() ++ additionalErrors ++ identitiesErrors
    )
  }

  private def resultFromMissingInput(seq:Either[String, Seq[VariableName]]) = seq match {
    case Right(inputs) => (inputs, Seq())
    case Left(ex) => (Seq(), Seq(ex))
  }

  @JSExport
  def showInForm(variable:VariableDefinition, executionResult:TemplateExecutionResult):Boolean =
    variable.varType(executionResult) match {
      case _:NoShowInForm => false
      case _ => true
    }

  @JSExport
  def isChoiceType(variable:VariableDefinition, executionResult:TemplateExecutionResult):Boolean = variable.varType(executionResult) match {
    case _:DefinedChoiceType => true
    case _ => false
  }

  @JSExport
  def isStructuredType(variable:VariableDefinition, executionResult:TemplateExecutionResult):Boolean = variable.varType(executionResult) match {
    case _:DefinedStructureType => true
    case _ => false
  }

  @JSExport
  def getChoiceValues(variable:VariableDefinition, executionResult: TemplateExecutionResult):js.Array[String] = variable.varType(executionResult) match {
    case choice:DefinedChoiceType => choice.choices.values.flatMap(_.evaluate(executionResult)).map(VariableType.convert[String]).toJSArray
    case _ => Seq().toJSArray
  }

  @JSExport
  def getStructureFieldDefinitions(variable:VariableDefinition, executionResult: TemplateExecutionResult):js.Array[VariableDefinition] = variable.varType(executionResult) match {
    case structure:DefinedStructureType =>
      structure.structure.names.map{ name => {
        val varType = structure.structure.typeDefinition(name)
        VariableDefinition(name, Some(VariableTypeDefinition(varType.name)))
      } }.toJSArray
    case _ => Seq().toJSArray
  }

  @JSExport
  def getStructureFieldValue(variable:VariableDefinition, field:VariableDefinition, structureValue:js.UndefOr[String], executionResult: TemplateExecutionResult):js.UndefOr[String] = variable.varType(executionResult) match {
    case structureType:DefinedStructureType =>
      val values = structureValue.map(structureType.cast(_, executionResult)).getOrElse(Map())
      (for {
        value <- values.get(field.name)
        fieldType <- structureType.structure.typeDefinition.get(field.name)
      } yield fieldType.internalFormat(value)).orUndefined

    case _ =>
      js.undefined
  }

  @JSExport
  def setStructureFieldValue(variable:VariableDefinition, fieldName:String, fieldValue:js.UndefOr[String], structureValue:js.UndefOr[String], executionResult: TemplateExecutionResult):js.UndefOr[String] = variable.varType(executionResult) match {
    case structure:DefinedStructureType =>
      structure.structure.typeDefinition.get(VariableName(fieldName)) match {
        case Some(fieldType) =>
          val currentMap = structureValue.map(structure.cast(_, executionResult)).getOrElse(Map())
          fieldValue.toOption match {
            case Some(value) =>
              val newMap = currentMap + (VariableName(fieldName) -> fieldType.cast(value, executionResult))
              structure.internalFormat(newMap)
            case None =>
              val newMap = currentMap - VariableName(fieldName)
              structure.internalFormat(newMap)
          }
        case None =>
          structureValue
      }
    case _ =>
      structureValue
  }

  @JSExport
  def getAddress(json:String):Address = AddressType.cast(json)

  @JSExport
  def getFormattedAddress(address:Address):String = address.formattedAddress

  @JSExport
  def noIdentity(result:ValidationResult):Boolean = result.identities.isEmpty

  @JSExport
  def missingIdentities(result:ValidationResult):Boolean = result.missingIdentities.nonEmpty

  @JSExport
  def hasMissingInputs(result:ValidationResult):Boolean = result.missingInputs.nonEmpty

  @JSExport
  def getMissingInputs(result:ValidationResult):js.Array[String] = result.missingInputs.map(_.name).distinct.toJSArray

  @JSExport
  def missingAllIdentities(result:ValidationResult):Boolean = result.identities.nonEmpty && result.missingIdentities.length === result.identities.length

  private def handleExecutionResult(executionResult:Either[String, TemplateExecutionResult]):js.Dictionary[Any] = executionResult match {
    case Right(result) =>
      result.state match {
        case ExecutionFinished =>
          js.Dictionary(
            "executionResult" -> result,
            "isError" -> false,
            "missingTemplate" -> false,
            "errorMessage" -> "")
        case ExecutionWaitForTemplate(_, definition) =>
          js.Dictionary(
            "executionResult" -> result,
            "isError" -> false,
            "missingTemplate" -> true,
            "missingTemplateName" -> definition.name.title,
            "errorMessage" -> s"the template ${definition.name} was not loaded")
        case _ =>
          js.Dictionary(
            "executionResult" -> result,
            "isError" -> true,
            "missingTemplate" -> false,
            "errorMessage" -> s"invalid end state ${result.state}")
      }
    case Left(ex) =>
      js.Dictionary(
        "executionResult" -> js.undefined,
        "isError" -> true,
        "errorMessage" -> ex)
  }

  @JSExport
  def getInitialParameters(executionResult:TemplateExecutionResult):js.Array[js.Dictionary[String]] = {
    executionResult.getAllVariables
      .filter({
        case (_, variable) => variable.varType(executionResult) match {
          case _:NoShowInForm => false
          case _ => true
        }}).filter({case (_, variable) => variable.defaultValue.exists(_ => true)})
      .map({ case (result, variable) => js.Dictionary(
        "name" -> variable.name.name,
        "value" -> getInitialParameter(variable, result))
      }).toJSArray
  }

  private def getInitialParameter(variable:VariableDefinition, executionResult: TemplateExecutionResult):String =
    variable.defaultValue
      .flatMap(variable.varType(executionResult).construct(_, executionResult))
      .map(variable.varType(executionResult).internalFormat).getOrElse("")


  @JSExport
  def getType(variable:VariableDefinition):String = variable.variableTypeDefinition.map(_.name).getOrElse(TextType.name)

  @JSExport
  def getDescription(variable:VariableDefinition):String = variable.description.getOrElse(variable.name.name)

  @JSExport
  def getName(variable:VariableDefinition):String = variable.name.name

  @JSExport
  def getTemplateName(templateDefinition: TemplateDefinition):String = templateDefinition.name.name.title

  @JSExport
  def getCleanName(variable:VariableDefinition):String = variable.name.name.replace(" ", "-")

  @JSExport
  def renderForReview(agreement:StructuredAgreement, jsOverriddenParagraphs:js.Dictionary[String]): String =
    render(agreement, Seq(), jsOverriddenParagraphs, markdown.forReview)

  @JSExport
  def renderForPreview(agreement:StructuredAgreement, hiddenVariables:js.Array[String], jsOverriddenParagraphs:js.Dictionary[String]): String =
    render(agreement, hiddenVariables, jsOverriddenParagraphs, markdown.forPreview)

  @JSExport
  def parseMarkdown(str:String):String =
    markdown.handleOverriddenParagraph(ReviewHtmlAgreementPrinter(), str)
        .paragraphFooter
        .result

  @JSExport
  def renderParagraphForEdit(agreement: StructuredAgreement, index:Int): String =
    markdown.forReviewEdit(agreement.paragraphs(index - 1))

  private def render(agreement:StructuredAgreement, hiddenVariables:Seq[String], jsOverriddenParagraphs:js.Dictionary[String], renderFunc:(StructuredAgreement, ParagraphEdits, Seq[String]) => String):String =
    renderFunc(agreement, prepareParagraphs(agreement, jsOverriddenParagraphs), hiddenVariables)

  @JSExport
  def checkValidity(variable:VariableDefinition, optValue:js.UndefOr[String], executionResult: TemplateExecutionResult): Any = optValue
    .flatMap(variable.varType(executionResult).cast(_, executionResult))

  @JSExport
  def getTypes:js.Array[String] =
    js.Array(VariableType.allTypes().map(_.name):_*)

  @JSExport
  def getExecutedVariables(executionResult:TemplateExecutionResult, jsDefinedValues:js.Dictionary[Any]): js.Array[VariableDefinition] = {
    getVariables(executionResult, executionResult.getExecutedVariables, prepareParameters(jsDefinedValues)).toJSArray
  }

  @JSExport
  def getVariables(executionResult:TemplateExecutionResult, jsDefinedValues:js.Dictionary[Any]): js.Array[VariableDefinition] = {
    getVariables(executionResult, executionResult.getAllVariableNames, prepareParameters(jsDefinedValues)).toJSArray
  }

  @JSExport
  def getAllConditionalVariableNames(executionResult:TemplateExecutionResult): js.Array[String] = {
    executionResult.getAllVariables
      .map({ case (_, variable) => variable})
      .filter(_.variableTypeDefinition === Some(VariableTypeDefinition(YesNoType.name)))
      .map(variable => variable.name.name).distinct.toJSArray
  }

  def getVariables(executionResult: TemplateExecutionResult, variables: Seq[VariableName], definedValues:TemplateParameters): Seq[VariableDefinition] = {
    val predefinedVariables = definedValues.params.keys.toSet
    variables
      .flatMap(name => executionResult.getVariable(name))
      .filter(_.varType(executionResult) match {
        case _:NoShowInForm => false
        case _ => true
      })
      .filter(variable => !predefinedVariables.contains(variable.name))
  }

  @JSExport
  def getAgreements(executionResult: TemplateExecutionResult):js.Array[js.Dictionary[Any]] =
    executionResult.agreements.map(agreement => {
      Dictionary[Any](
        "agreement" -> agreement,
        "executionResult" -> agreement.executionResult,
        "mainTemplate" -> agreement.mainTemplate,
        "showTitle" -> agreement.header.shouldShowTitle,
        "name" -> agreement.name,
        "title" -> agreement.title.title
      )
    }).toJSArray

  @JSExport
  def getIdentityEmail(identity:Identity):String = identity.email.email

  @JSExport
  def getIdentityId(identity:Identity):String = identity.userId.id

  @JSExport
  def createIdentityInternalValue(userId:js.UndefOr[String], email:String):String =
    IdentityType.internalFormat(createIdentity(userId, email))

  @JSExport
  def createIdentity(userId:js.UndefOr[String], email:String):Identity = {
    //FUTURE_WORK: implement this
    /*
    identifiers.map(obj => {
      val providerId = obj.selectDynamic("provider").toString
      val id = obj.selectDynamic("id").toString
      IdentityIdentifier(providerId, id)
    })
    */
    Identity(
      id = userId.toOption.map(UserId.apply),
      email = Email(email),
      identifiers = Seq(IdentityIdentifier("openlaw", email))
    )
  }

  @JSExport
  def getIdentityId(identity:js.UndefOr[Identity]):js.UndefOr[String] = identity.map(_.userId.id)

  @JSExport
  def getIdentities(validationResult: ValidationResult, executionResult: TemplateExecutionResult):js.Array[VariableDefinition] = {
    executionResult
      .getVariables(IdentityType)
      .map({case (_,variable) => variable.name -> variable})
      .toMap.values
      .filter(variable => validationResult.missingIdentities.contains(variable.name))
      .toJSArray
  }

  @JSExport
  def isSignatory(userId:String, executionResult: TemplateExecutionResult):Boolean = {
    executionResult
      .getVariableValues[Identity](IdentityType)
      .exists(_.id === Some(UserId(userId)))
  }

  @JSExport
  def getSections(document:TemplateExecutionResult):js.Array[String] =
    document.variableSectionList.toJSArray

  @JSExport
  def getVariableSections(document:TemplateExecutionResult):js.Dictionary[js.Array[String]] =
    document.sections
      .map({case (key,variables) => key -> variables.map(_.name).toJSArray}).toJSDictionary

  @JSExport
  def isDeal(template:CompiledTemplate):Boolean = template match {
    case _:CompiledDeal => true
    case _ => false
  }

  @JSExport
  def isHidden(variableDefinition: VariableDefinition):Boolean = variableDefinition.isHidden

  @JSExport
  def getCollectionSize(variable:VariableDefinition, value:String, executionResult: TemplateExecutionResult):Int = {
    getCollection(variable, executionResult, value).size
  }

  @JSExport
  def createVariableFromCollection(variable:VariableDefinition, index:Int, executionResult: TemplateExecutionResult):VariableDefinition = {
    variable.varType(executionResult) match {
      case collectionType:CollectionType =>
        VariableDefinition(VariableName(variable.name.name + "_" + index), variableTypeDefinition = Some(VariableTypeDefinition(collectionType.typeParameter.name)), description = Some(getDescription(variable)))

      case _ =>
        throw new RuntimeException(s"add element to collection only works for a variable of type Collection, not '${variable.varType(executionResult).name}'")
    }
  }

  @JSExport
  def addElementToCollection(variable:VariableDefinition, value:String, executionResult: TemplateExecutionResult):String = {
    val collection = getCollection(variable, executionResult, value)
    collection.collectionType.internalFormat(collection.copy(size = collection.size + 1))
  }

  @JSExport
  def setElementToCollection(optValue:js.UndefOr[String], index:Int, variable:VariableDefinition, collectionValue:String, executionResult: TemplateExecutionResult):String = {
    val collection = getCollection(variable, executionResult, collectionValue)
    optValue.toOption match {
      case Some(value) =>
        collection.collectionType.internalFormat(collection
          .copy(values = collection.values ++ Map(index -> collection.castValue(value, executionResult))))
      case None =>
        collection.collectionType.internalFormat(collection
          .copy(values = collection.values - index))
    }
  }

  @JSExport
  def removeElementFromCollection(index:Int, variable:VariableDefinition, executionResult: TemplateExecutionResult, value:String):String = {
    val collection = getCollection(variable, executionResult, value)

    val newValues = (collection.values - index).map({
      case (key,v) if key < index => key -> v
      case (key,v) => (key - 1) -> v
    })

    collection.collectionType.internalFormat(collection
      .copy(values = newValues, size = Math.max(collection.size - 1, 0))
    )
  }

  @JSExport
  def getCollectionElementValue(variable:VariableDefinition, executionResult: TemplateExecutionResult, value:String, index:Int):String = {
    val collection = getCollection(variable, executionResult, value)
    collection.values.get(index)
      .map(collection.valueInternalFormat)
      .getOrElse("")
  }

  @JSExport
  def getCollectionValue(variable:VariableDefinition, executionResult: TemplateExecutionResult, value:String):String = {
    val collection = getCollection(variable, executionResult, value)
    CollectionType(collection.collectionType).internalFormat(collection)
  }

  private def getCollection(variable:VariableDefinition, executionResult: TemplateExecutionResult, value:String):CollectionValue = {
    variable.varType(executionResult) match {
      case collectionType:CollectionType =>
        if(value.isEmpty) {
          CollectionValue(collectionType = collectionType)
        } else {
          VariableType.convert[CollectionValue](collectionType.cast(value, executionResult))
        }
      case _ =>
        throw new RuntimeException(s"add element to collection only works for a variable of type Collection, not '${variable.varType(executionResult).name}'")
    }
  }

  private def prepareParameters(jsParams:js.Dictionary[Any]):TemplateParameters = {
    val keys = jsParams.keys.toSeq
    val dynParams = jsParams.asInstanceOf[js.Dynamic]

    val params = keys
      .map(key => key -> dynParams.selectDynamic(key))
      .filter({case (_,value) => !js.isUndefined(value)})
      .map({case (key,value) => VariableName(key) -> value.toString})

    TemplateParameters(params.toMap)
  }

  private def prepareParagraphs(agreement:StructuredAgreement, jsParagraphs:js.Dictionary[String]):ParagraphEdits = {
    if(js.isUndefined(jsParagraphs)){
      ParagraphEdits()
    }else{
      val edits = agreement.paragraphs.indices
        .flatMap(index => jsParagraphs.get(index.toString).map(index -> _)).toMap

      ParagraphEdits(edits)
    }
  }
}

case class ValidationResult(
                             identities:Seq[VariableDefinition],
                             missingInputs:Seq[VariableName],
                             missingIdentities:Seq[VariableName],
                             validationExpressionErrors:Seq[String])
