package org.adridadou.openlaw.parser.template

import java.time.Clock

import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template.printers.{AgreementPrinter, XHtmlAgreementPrinter}
import org.adridadou.openlaw.parser.template.printers.XHtmlAgreementPrinter.FragsPrinter
import org.adridadou.openlaw.parser.template.variableTypes.YesNoType
import org.parboiled2.{ErrorFormatter, ParseError}
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt, handleFatalErrors}
import org.adridadou.openlaw.result.Implicits.RichTry

/**
  * Created by davidroon on 05.06.17.
  */
class OpenlawTemplateLanguageParserService(val internalClock:Clock) {

  def parseExpression(str: String): Result[Expression] = new ExpressionParser(str).root.run() match {
    case scala.util.Success(result) => Success(result)
    case scala.util.Failure(ex) => handleFatalErrors(ex)
  }

  def compileTemplate(source: String, clock: Clock = internalClock): Result[CompiledTemplate] = {
    val compiler = createTemplateCompiler(source, clock)

    attempt(compiler.rootRule.run().toResult).flatten match {
      case Failure(parseError: ParseError, _) => Failure(compiler.formatError(parseError, new ErrorFormatter(showTraces = true)))
      case Failure(ex, _) => handleFatalErrors(ex)
      case Success(result) => validate(result)
    }
  }

  private def validate(template: CompiledTemplate): Result[CompiledTemplate] = {
    variableTypesMap(template.block.elems, VariableRedefinition()) map template.withRedefinition
  }

  private def variableTypesMap(elems: Seq[TemplatePart], redefinition: VariableRedefinition): Result[VariableRedefinition] = {
    val initialValue: Result[VariableRedefinition] = Success(redefinition)
    elems.foldLeft(initialValue)((currentTypeMap, elem) => currentTypeMap.flatMap(variableTypesMap(elem, _)))
  }

  private def redefine(redefinition: VariableRedefinition, name: String, varTypeDefinition: VariableTypeDefinition, description: Option[String]): VariableRedefinition = {
    val typeMap = redefinition.typeMap + (name -> varTypeDefinition)
    val descriptions = description match {
      case Some(desc) => redefinition.descriptions + (name -> desc)
      case None => redefinition.descriptions
    }
    redefinition.copy(
      typeMap = typeMap,
      descriptions = descriptions)
  }

  private def variableTypesMap(elem: TemplatePart, redefinition: VariableRedefinition): Result[VariableRedefinition] = elem match {
    case variable: VariableDefinition if variable.nameOnly =>
      Success(redefinition)

    case variable: VariableDefinition =>
      redefinition.typeMap.get(variable.name.name) match {
        case _ if variable.isAnonymous => Right(redefinition)
        case Some(otherType) if variable.variableTypeDefinition.exists(_ === otherType) =>
          Success(redefinition)
        case Some(otherType) =>
          Failure(s"error mismatch for ${variable.name}. Was previously defined as $otherType but then as ${variable.variableTypeDefinition}")
        case None =>
          variable.variableTypeDefinition
            .map(typeDefinition => Success(redefine(redefinition, variable.name.name, typeDefinition, variable.description)))
            .getOrElse(Failure(s"${variable.name} is missing a variable type name"))
      }

    case _: VariableName =>
      Right(redefinition)

    case ConditionalBlock(block, elseBlock, conditionalExpression) =>
      val newTypeMap = conditionalExpression match {
        case variable: VariableDefinition =>
          redefinition.typeMap.get(variable.name.name) match {
            case Some(otherType) if otherType === VariableTypeDefinition(YesNoType.name) =>
              Success(redefinition)
            case Some(otherType) =>
              Failure(s"error mismatch for ${variable.name}. Was previously defined as $otherType but then as ${variable.variableTypeDefinition}")
            case None =>
              Success(redefine(redefinition, variable.name.name, VariableTypeDefinition(YesNoType.name), variable.description))
          }
        case _ =>
          Success(redefinition)
      }
      val newTypeMap2 = block.elems.foldLeft(newTypeMap)((currentTypeMap, elem) => currentTypeMap.flatMap(variableTypesMap(elem, _)))
      elseBlock.map(_.elems.foldLeft(newTypeMap2)((currentTypeMap, elem) => currentTypeMap.flatMap(variableTypesMap(elem, _)))).getOrElse(newTypeMap2)

    case CodeBlock(elems) =>
      variableTypesMap(elems, redefinition)

    case VariableSection(_, variables) =>
      variableTypesMap(variables, redefinition)

    case ConditionalBlockSet(blocks) =>
      val initialValue: Result[VariableRedefinition] = Success(redefinition)
      blocks.foldLeft(initialValue)((currentTypeMap, block) => currentTypeMap.flatMap(variableTypesMap(block, _)))

    case _ =>
      Success(redefinition)
  }

  def forPreview(structuredAgreement: StructuredAgreement, overriddenParagraphs: ParagraphEdits): String =
    XHtmlAgreementPrinter(preview = true, paragraphEdits = overriddenParagraphs).printParagraphs(structuredAgreement.paragraphs).print

  def forPreview(structuredAgreement: StructuredAgreement, overriddenParagraphs: ParagraphEdits, hiddenVariables: Seq[String]): String =
    XHtmlAgreementPrinter(preview = true, paragraphEdits = overriddenParagraphs, hiddenVariables = hiddenVariables).printParagraphs(structuredAgreement.paragraphs).print

  def forPreview(paragraph: Paragraph, variables: Seq[String]): String =
    XHtmlAgreementPrinter(preview = true, hiddenVariables = variables).printParagraphs(List(paragraph)).print

  def forReview(structuredAgreement: StructuredAgreement): String =
    forReview(structuredAgreement, ParagraphEdits())

  def forReview(structuredAgreement: StructuredAgreement, overriddenParagraphs: ParagraphEdits): String =
    XHtmlAgreementPrinter(preview = false, overriddenParagraphs, structuredAgreement.executionResult.getVariables.map(_.name.name)).printParagraphs(structuredAgreement.paragraphs).print

  def forReview(structuredAgreement: StructuredAgreement, overriddenParagraphs:ParagraphEdits, variables:Seq[String]):String =
    XHtmlAgreementPrinter(preview = false, overriddenParagraphs, variables).printParagraphs(structuredAgreement.paragraphs).print

  def forReview(paragraph: Paragraph, variables:Seq[String]):String =
    XHtmlAgreementPrinter(preview = false, hiddenVariables = variables).printParagraphs(List(paragraph)).print

  def forReviewEdit(paragraph:Paragraph):String =
    XHtmlAgreementPrinter(preview = false).printParagraphs(List(paragraph)).print

  def forReviewParagraph(str: String): String = MarkdownParser.parseMarkdown(str) match {
    case Left(ex) => throw new RuntimeException("error while parsing the markdown:" + ex)
    case Right(result) => XHtmlAgreementPrinter(preview = false).printFragments(result.map(FreeText).toList).print
  }

  def render[T](structuredAgreement: StructuredAgreement, overriddenParagraphs:ParagraphEdits, agreementPrinter: AgreementPrinter[T], hiddenVariables:Set[String]):AgreementPrinter[T] =
    structuredAgreement.paragraphs
    .foldLeft(agreementPrinter)({
      case (printer,paragraph) =>
        renderParagraph(paragraph, overriddenParagraphs, hiddenVariables, printer)
    })

  def handleOverriddenParagraph[T](p: AgreementPrinter[T], str: String):AgreementPrinter[T] = {
    val newP = MarkdownParser.parseMarkdown(str) match {
      case Left(ex) => throw new RuntimeException("error while parsing the markdown:" + ex)
      case Right(result) => result
        .foldLeft(p)({case (printer, elem) =>
          renderElement(FreeText(elem), Paragraph(), None, Set(), printer)
        })
    }
    newP.newState(p.state.copy(overriddenParagraphGenerated = true))
  }

  def getOrThrow(either:Either[String, CompiledTemplate]):CompiledTemplate = either match {
    case Right(compiledTemplate) => compiledTemplate
    case Left(ex) => throw new RuntimeException(ex)
  }

  private def renderParagraph[T](paragraph: Paragraph, overriddenParagraphs:ParagraphEdits, hiddenVariables:Set[String], agreementPrinter: AgreementPrinter[T]): AgreementPrinter[T] = {
    if(hasContent(paragraph)) {
      val p = agreementPrinter
          .paragraphStart()

      val optParagraph = overriddenParagraphs.edits.get(agreementPrinter.state.paragraphIndex)
      paragraph
        .elements.foldLeft(p)({case (printer, element) => renderElement(element, paragraph, optParagraph, hiddenVariables, printer)})
        .paragraphFooter.paragraphEnd()
    }else {
      paragraph
        .elements.foldLeft(agreementPrinter)({case (printer, element) => renderElement(element, paragraph, None, hiddenVariables, printer)})
    }
  }

  private def hasContent(paragraph:Paragraph):Boolean = paragraph.elements.exists({
    case _:FreeText => true
    case _:VariableElement => true
    case _:SectionElement => true
    case _ => false
  })

  private def renderElement[T](element: AgreementElement, docParagraph:Paragraph, optParagraph:Option[String], hiddenVariables:Set[String], agreementPrinter: AgreementPrinter[T]): AgreementPrinter[T] = {
    (element, optParagraph) match {
      case (table:TableElement, _) =>
        agreementPrinter.table(table) { (element: AgreementElement, printer: AgreementPrinter[T]) => renderElement(element, docParagraph, optParagraph, hiddenVariables, printer) }
      case (_:FreeText, _) if !agreementPrinter.state.headerGenerated =>
        renderElement(element, docParagraph, optParagraph, hiddenVariables, agreementPrinter.paragraphHeader(docParagraph))
      case (_:VariableElement, _) if !agreementPrinter.state.headerGenerated =>
        renderElement(element,docParagraph, optParagraph, hiddenVariables, agreementPrinter.paragraphHeader(docParagraph))
      case (_:FreeText,Some(paragraph)) if !agreementPrinter.state.overriddenParagraphGenerated =>
        handleOverriddenParagraph(agreementPrinter, paragraph)
      case (_:VariableElement, Some(paragraph)) if !agreementPrinter.state.overriddenParagraphGenerated =>
        handleOverriddenParagraph(agreementPrinter,  paragraph)
      case (_:SectionElement, Some(paragraph)) if !agreementPrinter.state.overriddenParagraphGenerated =>
        handleOverriddenParagraph(agreementPrinter, paragraph)
      case (_:FreeText, _) if agreementPrinter.state.overriddenParagraphGenerated =>
        agreementPrinter
      case (_:VariableElement,_) if agreementPrinter.state.overriddenParagraphGenerated =>
        agreementPrinter
      case (_:SectionElement,_) if agreementPrinter.state.overriddenParagraphGenerated =>
        agreementPrinter
      case (txt:FreeText,_) if agreementPrinter.state.conditionalDepth > 0 =>
        agreementPrinter.conditionalTextStart().text(txt.elem).conditionalTextEnd()
      case (txt:FreeText,_) =>
        agreementPrinter.text(txt.elem)
      case (image:ImageElement,_) =>
        agreementPrinter.image(image)
      case (link:Link,_) =>
        agreementPrinter.link(link)
      case (variable:VariableElement,_) if variable.dependencies.forall(variable => !hiddenVariables.contains(variable)) =>
        agreementPrinter.variableStart(variable.name)
        variable.content
          .foldLeft(agreementPrinter)((p, elem) => renderElement(elem, docParagraph, optParagraph, hiddenVariables, p))
            .variableEnd()
      case (variable:VariableElement,_) =>
        variable.content
          .foldLeft(agreementPrinter)((p, elem) => renderElement(elem, docParagraph, optParagraph, hiddenVariables, p))
      case (ConditionalStart(dependencies),_) if dependencies.forall(variable => !hiddenVariables.contains(variable)) =>
        agreementPrinter.conditionalStart()
      case (ConditionalEnd(dependencies),_) if dependencies.forall(variable => !hiddenVariables.contains(variable)) =>
        agreementPrinter.conditionalEnd()
      case (section:SectionElement, _) =>
        agreementPrinter
          .sectionStart(section)
          .paragraphHeader(docParagraph)
          .sectionHeader(section)
      case _ =>
        agreementPrinter
    }
  }

  private def createTemplateCompiler(markdown:String, clock:Clock):OpenlawTemplateLanguageParser = new OpenlawTemplateLanguageParser(markdown, clock)
}


case class VariableRedefinition(typeMap:Map[String, VariableTypeDefinition] = Map(), descriptions:Map[String,String] = Map())
