package org.adridadou.openlaw.parser.template

import java.time.Clock

import cats.implicits._
import org.adridadou.openlaw.{OpenlawBigDecimal, OpenlawBoolean}
import org.adridadou.openlaw.parser.template
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Failure, Result, Success}

import scala.annotation.tailrec

final case class CompiledAgreement(
  header:TemplateHeader = TemplateHeader(Map()),
  block: Block = Block(),
  redefinition:VariableRedefinition = VariableRedefinition(),
  clock: Clock = Clock.systemDefaultZone,
) extends CompiledTemplate {

  private val endOfParagraph = "(.)*[\\ |\t|\r]*\n[\\ |\t|\r]*\n[\\ |\t|\r|\n]*".r

  def structuredMainTemplate(executionResult:OpenlawExecutionState): Result[StructuredAgreement] =
    structured(executionResult, None, mainTemplate = true)

  def structuredInternal(executionResult: OpenlawExecutionState, path:Option[TemplatePath]): Result[StructuredAgreement] =
    structured(executionResult, path, mainTemplate = false)

  private def structured(executionResult: OpenlawExecutionState, path:Option[TemplatePath], mainTemplate:Boolean): Result[StructuredAgreement] =
    getAgreementElements(List(), block.elems.toList, executionResult).map { elements =>
      val paragraphs = cleanupParagraphs(generateParagraphs(elements))
      StructuredAgreement(
        executionResultId = executionResult.id,
        header = header,
        templateDefinition = executionResult.templateDefinition,
        path = path,
        mainTemplate = mainTemplate,
        paragraphs = paragraphs)
    }

  private def cleanupParagraphs(paragraphs: List[Paragraph]): List[Paragraph] =
    paragraphs.map(paragraph =>
      Paragraph(paragraph.elements.filter {
        case FreeText(elem) => !TextElement.isEmpty(elem)
        case _ => true
      })).filter(paragraph => paragraph.elements.nonEmpty)

  private def generateParagraphs(elements: Seq[AgreementElement]): List[Paragraph] =
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

  def handleText(paragraphBuilder: ParagraphBuilder, ftxt: FreeText): ParagraphBuilder =
    getParagraphs(ftxt.elem).foldLeft(paragraphBuilder)((builder, element) => element match {
      case ParagraphSeparator =>
        builder.closeLastParagraph()
      case _ =>
        builder.add(FreeText(element))
    })

  private def getParagraphs(te: TextElement): List[TextElement] =
    te match {
      case Text(text) =>
        val matches = endOfParagraph.findAllMatchIn(text).map(_.end)
        val (lastValue, newParagraphs) = matches.foldLeft((0, List.empty[TextElement])) {
          case ((lastIndex, p), endIndex) =>
            (endIndex, p :+ Text(text.substring(lastIndex, endIndex - 2)) :+ ParagraphSeparator)
        }
        newParagraphs :+ Text(text.substring(lastValue, text.length))
      case PageBreak =>  List(ParagraphSeparator, PageBreak, ParagraphSeparator)
      case SectionBreak => List(ParagraphSeparator, SectionBreak, ParagraphSeparator)
      case other =>  List(other)
    }

  @tailrec private def getAgreementElements(renderedElements:List[AgreementElement], elements:List[TemplatePart], executionResult: OpenlawExecutionState): Result[List[AgreementElement]] =
    elements match {
      case Nil =>
        Success(renderedElements)
      case element::rest =>
        getAgreementElementsFromElement(renderedElements, element, executionResult) match {
          case Success(list) => getAgreementElements(list, rest, executionResult)
          case f => f
        }
    }

  private def getAgreementElementsFromElement(renderedElements:List[AgreementElement], element:TemplatePart, executionResult: OpenlawExecutionState): Result[List[AgreementElement]] =
    element match {
      case t:Table =>
        for {
          headerElements <- t.header.map(entry => getAgreementElements(List(), entry, executionResult)).sequence
          rowElements <- t.rows.map(seq => seq.map(entry => getAgreementElements(List(), entry, executionResult)).sequence).sequence
        } yield {
          if (headerElements.isEmpty) {
            renderedElements.lastOption match {
              case Some(previousTable: TableElement) =>
                renderedElements.init :+ previousTable.copy(rows = previousTable.rows ++ rowElements)
              case _ =>
                renderedElements :+ TableElement(headerElements, rowElements)
            }
          } else {
            renderedElements :+ TableElement(headerElements, rowElements)
          }
        }

      case TemplateText(textElements) => getAgreementElements(renderedElements, textElements.toList, executionResult)
      case Text(str) => Success(renderedElements :+ FreeText(Text(str)))
      case Em => Success(renderedElements :+ FreeText(Em))
      case Strong => Success(renderedElements :+ FreeText(Strong))
      case Under => Success(renderedElements :+ FreeText(Under))
      case PageBreak => Success(renderedElements :+ FreeText(PageBreak))
      case SectionBreak => Success(renderedElements :+ FreeText(SectionBreak))
      case Indent => Success(renderedElements :+ FreeText(Indent))
      case Centered => Success(renderedElements :+ FreeText(Centered))
      case RightAlign => Success(renderedElements :+ FreeText(RightAlign))
      case RightThreeQuarters => Success(renderedElements :+ FreeText(RightThreeQuarters))
      case annotation: HeaderAnnotation => Success(renderedElements :+ annotation)
      case annotation: NoteAnnotation => Success(renderedElements :+ annotation)
      case variableDefinition:VariableDefinition if variableDefinition.isAnonymous =>
        val nbAnonymous = executionResult.processedAnonymousVariableCounter.getAndIncrement()
        getAgreementElementsFromElement(renderedElements, variableDefinition.copy(name = VariableName(executionResult.generateAnonymousName(nbAnonymous + 1))), executionResult)
      case variableDefinition:VariableDefinition if !variableDefinition.isHidden =>
        executionResult.getAliasOrVariableType(variableDefinition.name) match {
          case Right(variableType @ SectionType) =>
           getDependencies(variableDefinition.name, executionResult).flatMap { dependencies =>
             generateVariable(variableDefinition.name, Seq(), variableDefinition.formatter, executionResult).map { list =>
               renderedElements :+ VariableElement(variableDefinition.name, Some(variableType), list, dependencies)
             }
           }
          case Right(ClauseType) =>
            executionResult.subExecutionsInternal.get(variableDefinition.name) match {
              case Some(subExecution) =>
                getAgreementElements(renderedElements, subExecution.template.block.elems.toList, subExecution)
              case None =>
                Success(renderedElements)
            }

          case Right(_:NoShowInForm) =>
            Success(renderedElements)
          case Right(variableType) =>
           getDependencies(variableDefinition.name, executionResult).flatMap { dependencies =>
             generateVariable(variableDefinition.name, Seq(), variableDefinition.formatter, executionResult).map { list =>
               renderedElements :+ VariableElement(variableDefinition.name, Some(variableType), list, dependencies)
             }
           }
          case Left(_) =>
            // TODO: Should ignore failure?
            Success(renderedElements)
        }

      case ConditionalBlockSet(blocks) =>
        val result = blocks.toList.map { case block@ConditionalBlock(_, _, conditionalExpression) =>
          conditionalExpression.evaluate(executionResult).flatMap(_.map(VariableType.convert[OpenlawBoolean](_).map(boolean => block -> boolean)).sequence)
        }.sequence

        result.flatMap { booleans =>
          booleans
            .find {
              case Some((_, true)) => true
              case _ => false
            }
            .map {
              case Some((conditionalBlock, _)) => getAgreementElementsFromElement(renderedElements, conditionalBlock, executionResult)
              case x => Failure(s"unexpected value: $x")
            }
            .getOrElse(Success(renderedElements))
        }

      case ConditionalBlock(subBlock, elseBlock, conditionalExpression) =>

        conditionalExpression.evaluate(executionResult).flatMap(_.map(VariableType.convert[OpenlawBoolean](_)).sequence).flatMap {
          case Some(true) =>
            conditionalExpression.variables(executionResult).flatMap { variables =>
              val dependencies = variables.map(_.name)
              getAgreementElements(renderedElements ++ List(ConditionalStart(dependencies = dependencies)), subBlock.elems.toList, executionResult).map(_ ++ List(ConditionalEnd(dependencies)))
            }

          case _ =>
            conditionalExpression.variables(executionResult).flatMap { variables =>
              val dependencies = variables.map(_.name)
              elseBlock
                .map(block => getAgreementElements(renderedElements ++ List(ConditionalStart(dependencies = dependencies)), block.elems.toList, executionResult).map(_ ++ List(ConditionalEnd(dependencies))))
                .getOrElse(Success(renderedElements))
            }
        }

      case ForEachBlock(_, expression, subBlock) =>
        (for {
          valueOpt <- expression.evaluate(executionResult)
          list <- valueOpt.map(value => VariableType.convert[CollectionValue](value).map(_.list)).sequence
        } yield {
          val collection = list.getOrElse(Seq())
          collection.foldLeft(Success(renderedElements))((subElements, _) => {
            val subExecution = executionResult.finishedEmbeddedExecutions.remove(0)
            subElements.flatMap(getAgreementElements(_, subBlock.elems.toList, subExecution))
          })
        }).flatten

      case section @ Section(uuid, definition, lvl) =>
        val resetNumberingResult = definition
          .flatMap(_.parameters)
          .flatMap(_.parameterMap.toMap.get("numbering"))
          .map {
            case OneValueParameter(expr) =>
              expr
                .evaluate(executionResult)
                .flatMap(_.map(VariableType.convert[OpenlawBigDecimal](_).map(b => b.toInt)).sequence)
            case _ =>
              Success(None)
          }
          .sequence
          .map(_.flatten)

        val overrideSymbol = section.overrideSymbol(executionResult)
        val overrideFormat = section.overrideFormat(executionResult)
        val numberResult = executionResult
          .allProcessedSections
          .collectFirst { case (s, n) if s === section => Success(n) }
          .getOrElse(Failure(s"unexpected condition, section not found in processed sections"))

        (for {
          resetNumbering <- resetNumberingResult
          number <- numberResult
          value <- definition.flatMap(definition => executionResult.getVariable(definition.name)).flatMap(_.evaluate(executionResult).sequence).sequence
        } yield {
          value match {
            case Some(value: SectionInfo) =>
              for {
                overrideSymbolValue <- overrideSymbol
                overrideFormatValue <- overrideFormat
              } yield renderedElements.:+(SectionElement(value.numbering, lvl, number, resetNumbering, overrideSymbolValue, overrideFormatValue))

            case None =>
              val name = executionResult.sectionNameMapping(uuid)
              val result = executionResult
                .getVariable(name)
                .flatMap(_.evaluate(executionResult).sequence)
                .sequence

              result.flatMap { r =>
                r match {
                  case Some(v) =>
                    for {
                      overrideSymbolValue <- overrideSymbol
                      overrideFormatValue <- overrideFormat
                      info <- VariableType.convert[SectionInfo](v)
                    } yield renderedElements.:+(SectionElement(info.numbering, lvl, number, resetNumbering, overrideSymbolValue, overrideFormatValue))

                  case None =>
                    Failure("Section referenced before it has been rendered. The executor can't guess the section number before rendering it yet.")
                }
              }
            case Some(v) =>
              Failure(s"error while rendering sections the variable should be a section but is ${v.getClass.getSimpleName}")
          }
        }).flatten

      case VariableMember(name, keys, formatter) =>
        val definition = executionResult.getVariable(name).map(_.varType(executionResult))
        getDependencies(name, executionResult).flatMap { seq =>
          generateVariable(name, keys, formatter, executionResult).map { variable =>
            renderedElements.:+(VariableElement(name, definition, variable, seq))
          }
        }
      case _ =>
        Success(renderedElements)
    }

  private def getDependencies(name:VariableName, executionResult: TemplateExecutionResult): Result[Seq[String]] =
    executionResult.getAlias(name) match {
      case Some(alias) =>
        alias.variables(executionResult).map(_.map(_.name))
      case None => executionResult.getVariable(name) match {
        case Some(_) => Success(Seq(name.name))
        case None => Success(Seq())
      }
    }

  private def generateVariable(name: VariableName, keys:Seq[String], formatter:Option[FormatterDefinition], executionResult: TemplateExecutionResult): Result[List[AgreementElement]] =
    executionResult.getAliasOrVariableType(name).flatMap { varType =>
      val option = executionResult.getExpression(name).flatMap { expression =>
        expression.evaluate(executionResult).flatMap { valueOpt =>
          valueOpt.map { value =>
            varType
              .keysType(keys, expression, executionResult)
              .flatMap { keysType =>
                varType
                  .access(value, name, keys, executionResult)
                  .flatMap { option =>
                    option.map(value => keysType.format(formatter, value, executionResult).map(_.toList))
                      .getOrElse(Success(List(FreeText(Text(s"[[${expression.toString}]]")))))
                  }
              }
          }
          .sequence
        }
        .sequence
      }
      option.getOrElse(Success(varType.missingValueFormat(name).toList))
    }

  override def withRedefinition(redefinition: VariableRedefinition): CompiledAgreement = this.copy(redefinition = redefinition)
}
