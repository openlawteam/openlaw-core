package org.adridadou.openlaw.parser.template

import java.time.Clock

import cats.implicits._
import org.adridadou.openlaw.parser.template.variableTypes._

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
    val paragraphs = cleanupParagraphs(generateParagraphs(getAgreementElements(block.elems, executionResult)))
    StructuredAgreement(
      header = header,
      executionResult = executionResult,
      path = path,
      mainTemplate = mainTemplate,
      paragraphs = paragraphs)
  }

  private def cleanupParagraphs(paragraphs: List[Paragraph]): Vector[Paragraph] = paragraphs.map(paragraph =>
    Paragraph(paragraph.elements.filter {
      case FreeText(elem) => !TextElement.isEmpty(elem)
      case _ => true
    })).filter(paragraph => paragraph.elements.nonEmpty).toVector

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

  private def getAgreementElements(elements:Seq[TemplatePart], executionResult: TemplateExecutionResult):Vector[AgreementElement] = elements.foldLeft(Vector[AgreementElement]())((elems, elem) => elem match {
    case t:Table =>
      val headerElements = t.header.map(entry => getAgreementElements(entry, executionResult))
      val rowElements = t.rows.map(seq => seq.map(entry => getAgreementElements(entry, executionResult)))
      elems :+ TableElement(headerElements, rowElements)
    case TemplateText(textElement) => elems ++ getAgreementElements(textElement, executionResult)
    case Text(str) => elems :+ FreeText(Text(str))
    case Em => elems :+ FreeText(Em)
    case Strong => elems :+ FreeText(Strong)
    case PageBreak => elems :+ FreeText(PageBreak)
    case Indent => elems :+ FreeText(Indent)
    case Centered => elems :+ FreeText(Centered)
    case RightAlign => elems :+ FreeText(RightAlign)
    case RightThreeQuarters => elems :+ FreeText(RightThreeQuarters)
    case annotation: Annotation => elems :+ annotation
    case variableDefinition:VariableDefinition if !variableDefinition.isHidden =>
      executionResult.getAliasOrVariableType(variableDefinition.name) match {
        case Right(variableType @ SectionType) =>
          elems.:+(VariableElement(variableDefinition.name.name, Some(variableType), generateVariable(variableDefinition.name, Seq(), variableDefinition.formatter, executionResult), getDependencies(variableDefinition.name, executionResult)))
        case Right(_:NoShowInForm) =>
          elems
        case Right(variableType) =>
          elems.:+(VariableElement(variableDefinition.name.name, Some(variableType), generateVariable(variableDefinition.name, Seq(), variableDefinition.formatter, executionResult), getDependencies(variableDefinition.name, executionResult)))
        case Left(_) =>
          elems
      }

    case ConditionalBlockSet(blocks) =>
      blocks.find({
        case ConditionalBlock(_, conditionalExpression) => conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean])
      }) match {
        case Some(conditionalBlock) =>
          elems ++ getAgreementElements(Seq(conditionalBlock), executionResult)
        case None => elems
      }
    case ConditionalBlock(subBlock, conditionalExpression) if conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean]) =>
      val elements = getAgreementElements(subBlock.elems, executionResult)
      val dependencies = conditionalExpression.variables(executionResult).map(_.name)
      elems ++ Vector(ConditionalStart(dependencies = dependencies)) ++ elements ++ Vector(ConditionalEnd(dependencies))
    case ConditionalBlockSetWithElse(blocks) =>
      blocks.find({
        case ConditionalBlockWithElse(_, _, conditionalExpression) => conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean])
      }) match {
        case Some(conditionalBlock) =>
          elems ++ getAgreementElements(Seq(conditionalBlock), executionResult)
        case None => elems
      }
    case ConditionalBlockWithElse(subBlock, subBlock2, conditionalExpression) if conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean]) =>
      println("getAgreementElements " + subBlock)
      println("getAgreementElements " + subBlock2)
      val elements = getAgreementElements(subBlock.elems, executionResult)
      val dependencies = conditionalExpression.variables(executionResult).map(_.name)
      elems ++ Vector(ConditionalStartWithElse(dependencies = dependencies)) ++ elements ++ Vector(ConditionalEndWithElse(dependencies))
      val elements2 = getAgreementElements(subBlock2.elems, executionResult)
      elems ++ Vector(ConditionalStartWithElse(dependencies = dependencies)) ++ elements2 ++ Vector(ConditionalEndWithElse(dependencies))
    case ForEachBlock(_, expression, subBlock) =>
      val collection = expression.
        evaluate(executionResult)
        .map(value => VariableType.convert[CollectionValue](value).list)
        .getOrElse(Seq())

      elems ++ collection.foldLeft(Vector[AgreementElement]())((subElements, _) => {
        val subExecution = executionResult.finishedEmbeddedExecutions.remove(0)
        subElements ++ getAgreementElements(subBlock.elems, subExecution)
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
          elems.:+(SectionElement(value.numbering, lvl, number, resetNumbering, overrideSymbol, overrideFormat))

        case None =>
          val name = executionResult.sectionNameMapping(uuid)
          executionResult.getVariable(name)
            .flatMap(_.evaluate(executionResult)) match {
            case Some(value) =>
              val info = VariableType.convert[SectionInfo](value)
              elems.:+(SectionElement(info.numbering, lvl, number, resetNumbering, overrideSymbol, overrideFormat))
            case None =>
              throw new RuntimeException("Section referenced before it has been rendered. The executor can't guess the section number before rendering it yet.")
          }
        case Some(v) =>
          throw new RuntimeException(s"error while rendering sections the variable should be a section but is ${v.getClass.getSimpleName}")
      }

    case VariableMember(name, keys, formatter) =>
      val definition = executionResult.getVariable(name).map(_.varType(executionResult))
      elems.:+(VariableElement(name.name, definition, generateVariable(name, keys, formatter, executionResult), getDependencies(name, executionResult)))
    case _ =>
      elems
  })

  private def getDependencies(name:VariableName, executionResult: TemplateExecutionResult):Seq[String] = executionResult.getAlias(name) match {
    case Some(alias) =>
      alias.variables(executionResult).map(_.name)
    case None => executionResult.getVariable(name) match {
      case Some(_) => Seq(name.name)
      case None => Seq()
    }
  }

  private def generateVariable(name: VariableName, keys:Seq[String], formatter:Option[FormatterDefinition], executionResult: TemplateExecutionResult):Seq[AgreementElement] = {
    executionResult.getAliasOrVariableType(name).flatMap(varType => {
      val keysVarType:VariableType = varType.keysType(keys, executionResult)

      executionResult.getExpression(name).flatMap(_.evaluate(executionResult))
        .map(varType.access(_, keys, executionResult).flatMap(keysVarType.format(formatter, _, executionResult)))
        .getOrElse(Right(varType.missingValueFormat(name)))
    }) match {
      case Right(result) => result
      case Left(ex) => Seq(FreeText(Text(s"error: $ex")))
    }
  }

  override def withRedefinition(redefinition: VariableRedefinition): CompiledAgreement = this.copy(redefinition = redefinition)
}
