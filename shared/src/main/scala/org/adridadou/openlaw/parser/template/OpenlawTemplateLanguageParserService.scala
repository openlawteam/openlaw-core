package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.parser.template.printers.{
  AgreementPrinter,
  XHtmlAgreementPrinter
}
import org.adridadou.openlaw.parser.template.printers.XHtmlAgreementPrinter.FragsPrinter
import org.adridadou.openlaw.parser.template.variableTypes.YesNoType
import org.parboiled2.ParseError
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{
  Failure,
  Result,
  Success,
  attempt,
  handleFatalErrors
}
import org.adridadou.openlaw.result.Implicits.RichTry

/**
  * Created by davidroon on 05.06.17.
  */
class OpenlawTemplateLanguageParserService {

  def parseExpression(str: String): Result[Expression] =
    new ExpressionParser(str).root.run() match {
      case scala.util.Success(result) => Success(result)
      case scala.util.Failure(ex)     => handleFatalErrors(ex)
    }

  def compileTemplate(
      source: String
  ): Result[CompiledTemplate] = {
    val compiler = createTemplateCompiler(source)

    attempt(compiler.rootRule.run().toResult).flatten match {
      case Failure(parseError: ParseError, _) =>
        Failure(compiler.formatError(parseError))
      case Failure(ex, _)  => handleFatalErrors(ex)
      case Success(result) => validate(result)
    }
  }

  private def validate(template: CompiledTemplate): Result[CompiledTemplate] = {
    variableTypesMap(template.block.elems, VariableRedefinition()) map template.withRedefinition
  }

  private def variableTypesMap(
      elems: List[TemplatePart],
      redefinition: VariableRedefinition
  ): Result[VariableRedefinition] = {
    val initialValue: Result[VariableRedefinition] = Success(redefinition)
    elems.foldLeft(initialValue)((currentTypeMap, elem) =>
      currentTypeMap.flatMap(variableTypesMap(elem, _))
    )
  }

  private def redefine(
      redefinition: VariableRedefinition,
      name: String,
      varTypeDefinition: VariableTypeDefinition,
      description: Option[String]
  ): VariableRedefinition = {
    val typeMap = redefinition.typeMap + (name -> varTypeDefinition)
    val descriptions = description match {
      case Some(desc) => redefinition.descriptions + (name -> desc)
      case None       => redefinition.descriptions
    }
    redefinition.copy(typeMap = typeMap, descriptions = descriptions)
  }

  private def variableTypesMap(
      elem: TemplatePart,
      redefinition: VariableRedefinition
  ): Result[VariableRedefinition] = elem match {
    case variable: VariableDefinition if variable.nameOnly =>
      Success(redefinition)

    case variable: VariableDefinition =>
      redefinition.typeMap.get(variable.name.name) match {
        case _ if variable.isAnonymous => Right(redefinition)
        case Some(otherType)
            if variable.variableTypeDefinition.exists(_ === otherType) =>
          Success(redefinition)
        case Some(otherType) =>
          Failure(
            s"error mismatch for ${variable.name}. Was previously defined as $otherType but then as ${variable.variableTypeDefinition}"
          )
        case None =>
          variable.variableTypeDefinition
            .map(typeDefinition =>
              Success(
                redefine(
                  redefinition,
                  variable.name.name,
                  typeDefinition,
                  variable.description
                )
              )
            )
            .getOrElse(
              Failure(s"${variable.name} is missing a variable type name")
            )
      }

    case ConditionalBlock(block, elseBlock, conditionalExpression) =>
      val newTypeMap = conditionalExpression match {
        case variable: VariableDefinition =>
          redefinition.typeMap.get(variable.name.name) match {
            case Some(otherType)
                if otherType === VariableTypeDefinition(YesNoType.name) =>
              Success(redefinition)
            case Some(otherType) =>
              Failure(
                s"error mismatch for ${variable.name}. Was previously defined as $otherType but then as ${variable.variableTypeDefinition}"
              )
            case None =>
              Success(
                redefine(
                  redefinition,
                  variable.name.name,
                  VariableTypeDefinition(YesNoType.name),
                  variable.description
                )
              )
          }
        case _ =>
          Success(redefinition)
      }
      val newTypeMap2 =
        block.elems.foldLeft(newTypeMap)((currentTypeMap, elem) =>
          currentTypeMap.flatMap(variableTypesMap(elem, _))
        )
      elseBlock
        .map(
          _.elems.foldLeft(newTypeMap2)((currentTypeMap, elem) =>
            currentTypeMap.flatMap(variableTypesMap(elem, _))
          )
        )
        .getOrElse(newTypeMap2)

    case CodeBlock(elems) =>
      variableTypesMap(elems, redefinition)

    case VariableSection(_, variables) =>
      variableTypesMap(variables, redefinition)

    case ConditionalBlockSet(blocks) =>
      val initialValue: Result[VariableRedefinition] = Success(redefinition)
      blocks.foldLeft(initialValue)((currentTypeMap, block) =>
        currentTypeMap.flatMap(variableTypesMap(block, _))
      )

    case _ =>
      Success(redefinition)
  }

  def forPreview(
      structuredAgreement: StructuredAgreement
  ): String =
    XHtmlAgreementPrinter(
      preview = true
    ).printParagraphs(structuredAgreement.paragraphs).print

  def forReview(
      structuredAgreement: StructuredAgreement
  ): String =
    XHtmlAgreementPrinter(preview = false)
      .printParagraphs(structuredAgreement.paragraphs)
      .print

  def render[T](
      structuredAgreement: StructuredAgreement,
      agreementPrinter: AgreementPrinter[T]
  ): Result[AgreementPrinter[T]] =
    structuredAgreement.paragraphs
      .foldLeft(Success(agreementPrinter)) {
        case (result, paragraph) =>
          result.flatMap(printer =>
            renderParagraph(
              paragraph,
              printer
            )
          )
      }

  private def renderParagraph[T](
      paragraph: Paragraph,
      agreementPrinter: AgreementPrinter[T]
  ): Result[AgreementPrinter[T]] =
    if (hasContent(paragraph)) {
      val p = agreementPrinter
        .paragraphStart()

      paragraph.elements
        .foldLeft(Success(p)) {
          case (result, element) =>
            result.flatMap(printer =>
              renderElement(
                element,
                paragraph,
                printer
              )
            )
        }
        .map(_.paragraphFooter.paragraphEnd())
    } else {
      paragraph.elements
        .foldLeft(Success(agreementPrinter)) {
          case (result, element) =>
            result.flatMap(printer => renderElement(element, paragraph, printer)
            )
        }
    }

  private def hasContent(paragraph: Paragraph): Boolean =
    paragraph.elements.exists({
      case FreeText(Text(t)) if !t.trim.isEmpty => true
      case _: VariableElement                   => true
      case _: SectionElement                    => true
      case _                                    => false
    })

  private def renderElement[T](
      element: AgreementElement,
      docParagraph: Paragraph,
      agreementPrinter: AgreementPrinter[T]
  ): Result[AgreementPrinter[T]] = {
    element match {
      case table: TableElement =>
        Success(agreementPrinter.table(table) {
          (element: AgreementElement, printer: AgreementPrinter[T]) =>
            renderElement(
              element,
              docParagraph,
              printer
            )
        })
      case _: FreeText if !agreementPrinter.state.headerGenerated =>
        renderElement(
          element,
          docParagraph,
          agreementPrinter.paragraphHeader(docParagraph)
        )
      case _: VariableElement if !agreementPrinter.state.headerGenerated =>
        renderElement(
          element,
          docParagraph,
          agreementPrinter.paragraphHeader(docParagraph)
        )

      case txt: FreeText if agreementPrinter.state.conditionalDepth > 0 =>
        Success(
          agreementPrinter
            .conditionalTextStart()
            .text(txt.elem)
            .conditionalTextEnd()
        )
      case txt: FreeText =>
        Success(agreementPrinter.text(txt.elem))

      case placeholder: SignaturePlaceholder =>
        Success(agreementPrinter.signaturePlaceholder(placeholder.text))

      case image: ImageElement =>
        Success(agreementPrinter.image(image))
      case link: Link =>
        Success(agreementPrinter.link(link))
      case variable: VariableElement =>
        variable.content
          .foldLeft(Success(agreementPrinter.variableStart(variable.name)))(
            (result, elem) =>
              result.flatMap(p =>
                renderElement(
                  elem,
                  docParagraph,
                  p
                )
              )
          )
          .map(_.variableEnd())

      case ConditionalStart(_) =>
        Success(agreementPrinter.conditionalStart())
      case ConditionalEnd(_) =>
        Success(agreementPrinter.conditionalEnd())
      case section: SectionElement =>
        Success(
          agreementPrinter
            .sectionStart(section)
            .paragraphHeader(docParagraph)
            .sectionHeader(section)
        )
      case _ =>
        Success(agreementPrinter)
    }
  }

  private def createTemplateCompiler(
      markdown: String
  ): OpenlawTemplateLanguageParser =
    new OpenlawTemplateLanguageParser(markdown)
}

final case class VariableRedefinition(
    typeMap: Map[String, VariableTypeDefinition] = Map.empty,
    descriptions: Map[String, String] = Map.empty
)
