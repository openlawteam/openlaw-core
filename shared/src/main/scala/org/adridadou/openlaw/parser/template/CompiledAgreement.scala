package org.adridadou.openlaw.parser.template

import java.time.Clock

import cats.implicits._
import org.adridadou.openlaw.parser.template.variableTypes._

import scala.annotation.tailrec

case class CompiledAgreement(
  header:TemplateHeader,
  block: Block = Block(),
  redefinition:VariableRedefinition = VariableRedefinition(),
  clock: Clock = Clock.systemDefaultZone,
) extends CompiledTemplate {

  private val endOfParagraph = "(.)*[\\ |\t|\r]*\n[\\ |\t|\r]*\n[\\ |\t|\r|\n]*".r

  def structuredMainTemplate(executionResult:TemplateExecutionResult):StructuredAgreement =
    structured(executionResult, None, mainTemplate = true)

  def structuredInternal(executionResult: TemplateExecutionResult, path:Option[TemplatePath]):StructuredAgreement =
    structured(executionResult, path, mainTemplate = false)

  private def structured(executionResult: TemplateExecutionResult, path:Option[TemplatePath], mainTemplate:Boolean): StructuredAgreement = {
    val paragraphs = cleanupParagraphs(generateParagraphs(getAgreementElements(List(), block.elems.toList, executionResult)))
    StructuredAgreement(
      header = header,
      executionResult = executionResult,
      path = path,
      mainTemplate = mainTemplate,
      paragraphs = paragraphs)
  }

  private def cleanupParagraphs(paragraphs: List[Paragraph]): List[Paragraph] = paragraphs.map(paragraph =>
    Paragraph(paragraph.elements.filter {
      case FreeText(elem) => !TextElement.isEmpty(elem)
      case _ => true
    })).filter(paragraph => paragraph.elements.nonEmpty)

  private def generateParagraphs(elements: Seq[AgreementElement]): List[Paragraph] = {
    elements.foldLeft(ParagraphBuilder())({
      case (paragraphBuilder, element) => element match {
        case txt: FreeText =>
          handleText(paragraphBuilder, txt)
        case sectionElement:SectionElement =>
          paragraphBuilder
            .closeLastParagraph()
            .add(sectionElement)
        case _ =>
          paragraphBuilder.add(element)
      }
    }).build
  }

  def handleText(paragraphBuilder: ParagraphBuilder, ftxt: FreeText): ParagraphBuilder = {
    getParagraphs(ftxt.elem).foldLeft(paragraphBuilder)((builder, element) => element match {
      case ParagraphSeparator =>
        builder.closeLastParagraph()
      case _ =>
        builder.add(FreeText(element))
    })
  }

  private def getParagraphs(te: TextElement): List[TextElement] = te match {
    case Text(text) =>
      val matches = endOfParagraph.findAllMatchIn(text).map(_.end)
      val (lastValue, newParagraphs) = matches.foldLeft((0, List.empty[TextElement])) {
        case ((lastIndex, p), endIndex) =>
          (endIndex, p :+ Text(text.substring(lastIndex, endIndex - 2)) :+ ParagraphSeparator)
      }
      newParagraphs :+ Text(text.substring(lastValue, text.length))
    case PageBreak =>  List(ParagraphSeparator, PageBreak, ParagraphSeparator)
    case other =>  List(other)
  }

  @tailrec private def getAgreementElements(renderedElements:List[AgreementElement], elements:List[TemplatePart], executionResult: TemplateExecutionResult):List[AgreementElement] = {
    elements match {
      case Nil =>
        renderedElements
      case element::rest =>
        getAgreementElements(getAgreementElementsFromElement(renderedElements, element, executionResult), rest, executionResult)
    }
  }

  private def getAgreementElementsFromElement(renderedElements:List[AgreementElement], element:TemplatePart, executionResult: TemplateExecutionResult):List[AgreementElement] = {
    element match {
      case t:Table =>
        val headerElements = t.header.map(entry => getAgreementElements(List(), entry, executionResult))
        val rowElements = t.rows.map(seq => seq.map(entry => getAgreementElements(List(), entry, executionResult)))
        if(headerElements.isEmpty ) {
          renderedElements.lastOption match {
            case Some(previousTable:TableElement) =>
              renderedElements.init :+ previousTable.copy(rows = previousTable.rows ++ rowElements)
            case _ =>
              renderedElements :+ TableElement(headerElements, rowElements)
          }
        } else {
          renderedElements :+ TableElement(headerElements, rowElements)
        }

      case TemplateText(textElements) => getAgreementElements(renderedElements, textElements.toList, executionResult)
      case Text(str) => renderedElements :+ FreeText(Text(str))
      case Em => renderedElements :+ FreeText(Em)
      case Strong => renderedElements :+ FreeText(Strong)
      case PageBreak => renderedElements :+ FreeText(PageBreak)
      case Indent => renderedElements :+ FreeText(Indent)
      case Centered => renderedElements :+ FreeText(Centered)
      case RightAlign => renderedElements :+ FreeText(RightAlign)
      case RightThreeQuarters => renderedElements :+ FreeText(RightThreeQuarters)
      case annotation: HeaderAnnotation => renderedElements :+ annotation
      case annotation: NoteAnnotation => renderedElements :+ annotation
      case variableDefinition:VariableDefinition if !variableDefinition.isHidden =>
        executionResult.getAliasOrVariableType(variableDefinition.name) match {
          case Right(variableType @ SectionType) =>
            renderedElements.:+(VariableElement(variableDefinition.name.name, Some(variableType), generateVariable(variableDefinition.name, Seq(), variableDefinition.formatter, executionResult), getDependencies(variableDefinition.name, executionResult)))
          case Right(_:NoShowInForm) =>
            renderedElements
          case Right(variableType) =>
            renderedElements.:+(VariableElement(variableDefinition.name.name, Some(variableType), generateVariable(variableDefinition.name, Seq(), variableDefinition.formatter, executionResult), getDependencies(variableDefinition.name, executionResult)))
          case Left(_) =>
            renderedElements
        }

      case ConditionalBlockSet(blocks) =>
        blocks.find({
          case ConditionalBlock(_,_, conditionalExpression) => conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean])
        }) match {
          case Some(conditionalBlock) =>
            getAgreementElementsFromElement(renderedElements, conditionalBlock, executionResult)
          case None => renderedElements
        }
      case ConditionalBlock(subBlock, _, conditionalExpression) if conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean]) =>
        val dependencies = conditionalExpression.variables(executionResult).map(_.name)
        getAgreementElements(renderedElements ++ List(ConditionalStart(dependencies = dependencies)), subBlock.elems.toList, executionResult) ++ List(ConditionalEnd(dependencies))

      case ConditionalBlock(_, elseBlock, conditionalExpression) =>
        val dependencies = conditionalExpression.variables(executionResult).map(_.name)
        elseBlock
          .map(block => getAgreementElements(renderedElements ++ List(ConditionalStart(dependencies = dependencies)), block.elems.toList, executionResult) ++ List(ConditionalEnd(dependencies)))
          .getOrElse(renderedElements)

      case ForEachBlock(_, expression, subBlock) =>
        val collection = expression.
          evaluate(executionResult)
          .map(value => VariableType.convert[CollectionValue](value).list)
          .getOrElse(Seq())

        collection.foldLeft(renderedElements)((subElements, _) => {
          val subExecution = executionResult.finishedEmbeddedExecutions.remove(0)
          getAgreementElements(subElements, subBlock.elems.toList, subExecution)
        })
      case section @ Section(uuid, definition, lvl) =>
        val resetNumbering = definition
          .flatMap(_.parameters)
          .flatMap(_.parameterMap.toMap.get("numbering"))
          .flatMap({
            case OneValueParameter(expr) => expr.evaluate(executionResult).map(VariableType.convert[BigDecimal]).map(_.toInt)
            case _ => None
          })

        val overrideSymbol = section.overrideSymbol(executionResult)
        val overrideFormat = section.overrideFormat(executionResult)
        val number = executionResult
          .allProcessedSections
          .collectFirst { case (s, n) if s === section => n }
          .getOrElse(throw new RuntimeException(s"unexpected condition, section not found in processed sections"))

        definition
          .flatMap(definition => executionResult.getVariable(definition.name))
          .flatMap(_.evaluate(executionResult)) match {
          case Some(value:SectionInfo) =>
            renderedElements.:+(SectionElement(value.numbering, lvl, number, resetNumbering, overrideSymbol, overrideFormat))

          case None =>
            val name = executionResult.sectionNameMapping(uuid)
            executionResult.getVariable(name)
              .flatMap(_.evaluate(executionResult)) match {
              case Some(value) =>
                val info = VariableType.convert[SectionInfo](value)
                renderedElements.:+(SectionElement(info.numbering, lvl, number, resetNumbering, overrideSymbol, overrideFormat))
              case None =>
                throw new RuntimeException("Section referenced before it has been rendered. The executor can't guess the section number before rendering it yet.")
            }
          case Some(v) =>
            throw new RuntimeException(s"error while rendering sections the variable should be a section but is ${v.getClass.getSimpleName}")
        }

      case VariableMember(name, keys, formatter) =>
        val definition = executionResult.getVariable(name).map(_.varType(executionResult))
        renderedElements.:+(VariableElement(name.name, definition, generateVariable(name, keys, formatter, executionResult), getDependencies(name, executionResult)))
      case _ =>
        renderedElements
    }
  }

  private def getDependencies(name:VariableName, executionResult: TemplateExecutionResult):Seq[String] = executionResult.getAlias(name) match {
    case Some(alias) =>
      alias.variables(executionResult).map(_.name)
    case None => executionResult.getVariable(name) match {
      case Some(_) => Seq(name.name)
      case None => Seq()
    }
  }

  private def generateVariable(name: VariableName, keys:Seq[String], formatter:Option[FormatterDefinition], executionResult: TemplateExecutionResult):List[AgreementElement] = {
    executionResult.getAliasOrVariableType(name).flatMap(varType => {
      val keysVarType:VariableType = varType.keysType(keys, executionResult)

      executionResult.getExpression(name).flatMap(_.evaluate(executionResult))
        .map(varType.access(_, keys, executionResult).flatMap(keysVarType.format(formatter, _, executionResult)))
        .getOrElse(Right(varType.missingValueFormat(name)))
    }) match {
      case Right(result) => result.toList
      case Left(ex) => List(FreeText(Text(s"error: $ex")))
    }
  }

  override def withRedefinition(redefinition: VariableRedefinition): CompiledAgreement = this.copy(redefinition = redefinition)
}
