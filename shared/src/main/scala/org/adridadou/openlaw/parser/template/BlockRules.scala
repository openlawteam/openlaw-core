package org.adridadou.openlaw.parser.template

import cats.implicits._
import java.util.UUID

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.YesNoType
import org.parboiled2.{Parser, Rule0, Rule1}

import scala.annotation.tailrec

/**
  * Created by davidroon on 06.06.17.
  */
trait BlockRules extends Parser with ExpressionRules with GlobalRules {

  def blockRule: Rule1[Block] = rule {
    zeroOrMore(
      centeredLine | rightThreeQuartersLine | rightLine | pageBreak | sectionBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | expressionKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | headerAnnotationPart | noteAnnotationPart | textPart
    ) ~> ((s: Seq[TemplatePart]) => Block(s.toList))
  }

  def blockInConditionalRule: Rule1[Block] = rule {
    zeroOrMore(
      centeredLine | rightThreeQuartersLine | rightLine | pageBreak | sectionBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | expressionKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | headerAnnotationPart | noteAnnotationPart | textPartNoColons
    ) ~> ((s: Seq[TemplatePart]) => Block(s.toList))
  }
  def blockInConditionalElseRule: Rule1[Block] = rule {
    zeroOrMore(
      centeredLine | rightThreeQuartersLine | rightLine | pageBreak | sectionBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | expressionKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | headerAnnotationPart | noteAnnotationPart | textPartNoColons
    ) ~> ((s: Seq[TemplatePart]) => Block(s.toList))
  }

  def blockNoStrong: Rule1[Block] = rule {
    zeroOrMore(
      centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | headerAnnotationPart | noteAnnotationPart | textPartNoStrong
    ) ~> ((s: Seq[TemplatePart]) => Block(s.toList))
  }

  def blockNoEm: Rule1[Block] = rule {
    zeroOrMore(
      centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | headerAnnotationPart | noteAnnotationPart | textPartNoEm
    ) ~> ((s: Seq[TemplatePart]) => Block(s.toList))
  }

  def blockNoUnder: Rule1[Block] = rule {
    zeroOrMore(
      centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | headerAnnotationPart | noteAnnotationPart | textPartNoUnder
    ) ~> ((s: Seq[TemplatePart]) => Block(s.toList))
  }

  def blockNoStrongNoEmNoUnder: Rule1[Block] = rule {
    zeroOrMore(
      centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | headerAnnotationPart | noteAnnotationPart | textPartNoStrongNoEmNoUnder
    ) ~> ((s: Seq[TemplatePart]) => Block(s.toList))
  }

  def conditionalBlockSetKey: Rule1[ConditionalBlockSet] = rule {
    openB ~ oneOrMore(ws ~ conditionalBlockKey ~ ws) ~ closeB ~> (
        (blocks: Seq[ConditionalBlock]) => ConditionalBlockSet(blocks.toList)
    )
  }

  def foreachBlockKey: Rule1[ForEachBlock] = rule { &(openB) ~ foreachBlock }

  def foreachBlock: Rule1[ForEachBlock] = rule {
    openB ~ "#for each" ~ variableName ~ ws ~ ":" ~ ws ~ ExpressionRule ~ ws ~ "=>" ~ ws ~ blockRule ~ closeB ~> (
        (
            variable: VariableName,
            expression: Expression,
            block: Block
        ) => ForEachBlock(variable, expression, block)
    )
  }

  def conditionalBlockKey: Rule1[ConditionalBlock] = rule {
    &(openB) ~ conditionalBlock
  }

  def conditionalBlock: Rule1[ConditionalBlock] = rule {
    openB ~ ws ~ conditionalExpressionRule ~ optional(ws ~ "=>") ~ ws ~ blockInConditionalRule ~ optional(
      conditionalBlockElse
    ) ~ closeB ~> (
        (
            expression: Expression,
            block: Block,
            elseBlock: Option[Block]
        ) => ConditionalBlock(block, elseBlock, expression)
    )
  }

  def conditionalExpressionRule: Rule1[Expression] = rule {
    ExpressionRule ~> (
        (expr: Expression) =>
          expr match {
            case variable: VariableDefinition =>
              variable.copy(variableTypeDefinition =
                Some(VariableTypeDefinition(YesNoType.name))
              )
            case name: VariableName =>
              VariableDefinition(
                name,
                Some(VariableTypeDefinition(YesNoType.name)),
                None
              )
            case _ => expr
          }
      )
  }

  def conditionalBlockElse: Rule1[Block] = rule {
    "::" ~ ws ~ blockInConditionalElseRule
  }

  def codeBlockKey: Rule1[CodeBlock] = rule { &(openA) ~ codeBlock }

  def codeBlock: Rule1[CodeBlock] = rule {
    openA ~ zeroOrMore(
      ws ~ (varAliasKey | varMemberKey | varKey | comment | variableSectionKey) ~ ws
    ) ~ closeA ~> ((s: Seq[TemplatePart]) => CodeBlock(s.toList))
  }

  def comment: Rule1[EmptyTemplatePart.type] = rule {
    &("#" | "//") ~ capture(commentsChar) ~ "\n" ~> (
        (s: String) => EmptyTemplatePart
    )
  }

  def sectionKey: Rule1[Section] = rule {
    &(sectionChar) ~ section ~ optional(&(sectionBreak))
  }

  def section: Rule1[Section] = rule {
    capture(oneOrMore("^")) ~ optional(sectionDefinition) ~ optional(
      capture(zeroOrMore("\\sectionbreak"))
    ) ~> (
        (
            elems: String,
            namedSection: Option[SectionDefinition],
            _: Option[String]
        ) => Section(UUID.randomUUID().toString, namedSection, elems.length)
    )
  }

  def sectionDefinition: Rule1[SectionDefinition] = rule {
    "(" ~ charsKeyAST ~ optional("(" ~ parametersMapDefinition ~ ")") ~ ")" ~> (
        (
            name: String,
            params: Option[Parameters]
        ) => SectionDefinition(name, params)
    )
  }

  def variableSectionKey: Rule1[VariableSection] = rule {
    &(variableSectionChar) ~ variableSection
  }

  def variableSection: Rule1[VariableSection] = rule {
    variableSectionChar ~ capture(characters) ~ variableSectionChar ~ ws ~ oneOrMore(
      wsNoReturn ~ variable ~ wsNoReturn
    ).separatedBy("\n") ~> (
        (
            name: String,
            variables: Seq[VariableDefinition]
        ) => VariableSection(name, variables.toList)
    )
  }

  def centeredLine: Rule1[TemplateText] = rule {
    capture(centered) ~ optional(ws) ~> (
        (_: String) => TemplateText(List(Centered))
    )
  }

  def rightLine: Rule1[TemplateText] = rule {
    capture(right) ~ optional(ws) ~> (
        (_: String) => TemplateText(List(RightAlign))
    )
  }

  def rightThreeQuartersLine: Rule1[TemplateText] = rule {
    capture(rightThreeQuarters) ~ optional(ws) ~> (
        (_: String) => TemplateText(List(RightThreeQuarters))
    )
  }

  def pageBreak: Rule1[TemplateText] = rule {
    capture(pagebreak) ~ optional(ws) ~> (
        (_: String) => TemplateText(List(PageBreak))
    )
  }

  def sectionBreak: Rule1[TemplateText] = rule {
    capture(sectionbreak) ~ optional(ws) ~> (
        (_: String) => TemplateText(List(SectionBreak))
    )
  }

  def indentLine: Rule1[TemplateText] = rule {
    capture(indent) ~ optional(ws) ~> (
        (_: String) => TemplateText(List(Indent))
    )
  }

  def textPart: Rule1[TemplateText] = rule {
    textElement ~> ((s: Seq[TemplatePart]) => TemplateText(s.toList))
  }

  def textPartNoColons: Rule1[TemplateText] = rule {
    textElementNoColons ~> ((s: Seq[TemplatePart]) => TemplateText(s.toList))
  }

  def textPartNoStrong: Rule1[TemplateText] = rule {
    textElementNoStrong ~> ((s: Seq[TemplatePart]) => TemplateText(s.toList))
  }

  def textPartNoEm: Rule1[TemplateText] = rule {
    textElementNoEm ~> ((s: Seq[TemplatePart]) => TemplateText(s.toList))
  }

  def textPartNoUnder: Rule1[TemplateText] = rule {
    textElementNoUnder ~> ((s: Seq[TemplatePart]) => TemplateText(s.toList))
  }

  def textPartNoStrongNoEmNoUnder: Rule1[TemplateText] = rule {
    textNoReturn ~> ((s: Seq[TemplatePart]) => TemplateText(s.toList))
  }

  def textElement: Rule1[List[TemplatePart]] = rule {
    tableKey | strongWord | emWord | underWord | text | pipeText | starText | underLineText
  }

  def textElementNoStrong: Rule1[Seq[TemplatePart]] = rule {
    innerEmWord | innerUnderWord | textNoReturn | pipeText | underLineText
  }

  def textElementNoColons: Rule1[List[TemplatePart]] = rule {
    tableKey | strongWord | emWord | underWord | textNoColons | pipeText | starText | underLineText
  }

  def textElementNoEm: Rule1[Seq[TemplatePart]] = rule {
    innerStrongWord | innerUnderWord | textNoReturn | pipeText | underLineText
  }

  def textElementNoUnder: Rule1[Seq[TemplatePart]] = rule {
    innerStrongWord | innerEmWord | textNoReturn | pipeText | starText
  }

  def twoStar: Rule0 = rule(strong)
  def twoStarcontents: Rule1[Block] = rule { !twoStar ~ blockNoStrong }
  def innerTwoStarcontents: Rule1[Block] = rule {
    !twoStar ~ blockNoStrongNoEmNoUnder
  }

  def strongWord: Rule1[List[TemplatePart]] = rule {
    twoStar ~ twoStarcontents ~ twoStar ~> (
        (block: Block) => List(Strong) ++ block.elems ++ List(Strong)
    )
  }
  def innerStrongWord: Rule1[List[TemplatePart]] = rule {
    twoStar ~ innerTwoStarcontents ~ twoStar ~> (
        (block: Block) => List(Strong) ++ block.elems ++ List(Strong)
    )
  }

  def oneStar: Rule0 = rule(em)
  def oneStarcontents: Rule1[List[TemplatePart]] = rule {
    !oneStar ~ blockNoEm ~> ((block: Block) => block.elems)
  }
  def innerOneStarcontents: Rule1[List[TemplatePart]] = rule {
    !oneStar ~ blockNoStrongNoEmNoUnder ~> ((block: Block) => block.elems)
  }

  def emWord: Rule1[List[TemplatePart]] = rule {
    oneStar ~ oneStarcontents ~ oneStar ~> (
        (elems: List[TemplatePart]) => List(Em) ++ elems ++ List(Em)
    )
  }
  def innerEmWord: Rule1[List[TemplatePart]] = rule {
    oneStar ~ innerOneStarcontents ~ oneStar ~> (
        (elems: List[TemplatePart]) => List(Em) ++ elems ++ List(Em)
    )
  }

  def underLines: Rule0 = rule(under)
  def underLinescontents: Rule1[List[TemplatePart]] = rule {
    !underLines ~ blockNoUnder ~> ((block: Block) => block.elems)
  }
  def innerUnderLinescontents: Rule1[List[TemplatePart]] = rule {
    !underLines ~ blockNoStrongNoEmNoUnder ~> ((block: Block) => block.elems)
  }

  def underWord: Rule1[List[TemplatePart]] = rule {
    underLines ~ underLinescontents ~ underLines ~> (
        (elems: List[TemplatePart]) => List(Under) ++ elems ++ List(Under)
    )
  }
  def innerUnderWord: Rule1[List[TemplatePart]] = rule {
    underLines ~ underLinescontents ~ underLines ~> (
        (elems: List[TemplatePart]) => List(Under) ++ elems ++ List(Under)
    )
  }

  def text: Rule1[List[TemplatePart]] = rule {
    capture(characters) ~> ((s: String) => List(Text(TextCleaning.dots(s))))
  }

  def textNoReturn: Rule1[List[TemplatePart]] = rule {
    capture(charactersNoReturn) ~> (
        (s: String) => List(Text(TextCleaning.dots(s)))
    )
  }

  def textNoColons: Rule1[List[TemplatePart]] = rule {
    capture(charactersNoColons) ~> (
        (s: String) => List(Text(TextCleaning.dots(s)))
    )
  }

  def starText: Rule1[List[TemplatePart]] = rule {
    capture(em) ~> ((s: String) => List(Text(TextCleaning.dots(s))))
  }

  def underLineText: Rule1[List[TemplatePart]] = rule {
    capture(under) ~> ((s: String) => List(Text(TextCleaning.dots(s))))
  }

  def pipeText: Rule1[List[TemplatePart]] = rule {
    capture(pipe) ~> ((s: String) => List(Text(TextCleaning.dots(s))))
  }

  def tableText: Rule1[TemplatePart] = rule {
    capture(normalCharNoReturn) ~> ((s: String) => Text(s))
  }

  // the table parsing construct below may return empty whitespace at the end of the cell, this trims it and accumulates text nodes
  @tailrec final def accumulateTextAndTrim(
      seq: Seq[TemplatePart],
      accu: Seq[TemplatePart] = Seq()
  ): Seq[TemplatePart] = seq match {
    case Seq() => accu
    case seq @ Seq(head, tail @ _*) =>
      val texts = seq
        .takeWhile({
          case _: Text => true
          case _       => false
        })
        .map({
          case t: Text => t.str
          case _       => ""
        })

      val trimmed = texts.foldLeft("")(_ + _).trim
      texts.size match {
        case 0 => accumulateTextAndTrim(tail, accu :+ head)
        case _ if trimmed === "" =>
          accumulateTextAndTrim(seq.drop(texts.size), accu)
        case x => accumulateTextAndTrim(seq.drop(x), accu :+ Text(trimmed))
      }
  }

  def whitespace: Rule0 = rule { zeroOrMore(anyOf(tabOrSpace)) }
  def tableColumnEntryBlock: Rule1[Seq[TemplatePart]] = rule {
    oneOrMore(
      varAliasKey | varKey | varMemberKey | conditionalBlockSetKey | conditionalBlockKey | foreachBlockKey | tableText
    ) ~> ((seq: Seq[TemplatePart]) => accumulateTextAndTrim(seq))
  }

  def tableKey: Rule1[List[Table]] = rule {
    &(pipe) ~ table ~> ((t: Table) => List(t))
  }

  def table: Rule1[Table] = rule {
    tableWithHeaderAndRow | tableWithoutRow | tableWithoutHeader
  }

  def EndOfBlock: Rule0 = rule {
    nl | &(closeB) | EOI
  }

  def tableWithHeaderAndRow: Rule1[Table] = rule {
    tableRow ~ whitespace ~ nl ~ tableHeaderBreak ~ whitespace ~ nl ~ whitespace ~ oneOrMore(
      tableRow ~ EndOfBlock
    ) ~> (
        (
            headers: List[List[TemplatePart]],
            alignment: Seq[(Alignment, Border)],
            rows: Seq[List[List[TemplatePart]]]
        ) =>
          Table(
            headers,
            alignment.toList,
            rows.toList
          )
      )
  }

  def tableWithoutHeader: Rule1[Table] = rule {
    optional(tableHeaderBreak ~ nl) ~ whitespace ~ oneOrMore(
      tableRow ~ EndOfBlock
    ) ~> (
        (
            styling: Option[Seq[(Alignment, Border)]],
            rows: Seq[Seq[Seq[TemplatePart]]]
        ) =>
          Table(
            Nil,
            styling.map(_.toList).getOrElse(Nil),
            rows.map(_.map(_.toList).toList).toList
          )
      )
  }

  def tableWithoutRow: Rule1[Table] = rule {
    tableRow ~ nl ~ whitespace ~ tableHeaderBreak ~ whitespace ~ EndOfBlock ~> (
        (
            headers: List[List[TemplatePart]],
            alignment: Seq[(Alignment, Border)]
        ) => Table(headers, alignment.toList, Nil)
    )
  }

  def tableHeaderBreak: Rule1[Seq[(Alignment, Border)]] = rule {
    whitespace ~ pipe ~ oneOrMore(tableHeaderBreakString)
      .separatedBy(pipe) ~ whitespace ~ pipe ~ whitespace
  }

  def tableColumnEntry: Rule1[Seq[TemplatePart]] = rule {
    whitespace ~ tableColumnEntryBlock ~ whitespace
  }

  def tableRow: Rule1[List[List[TemplatePart]]] = rule {
    whitespace ~ pipe ~ (tableColumnEntry ~ pipe ~ oneOrMore(tableColumnEntry)
      .separatedBy(pipe) ~ pipe ~ whitespace ~> (
        (
            row: Seq[TemplatePart],
            remaining: Seq[Seq[TemplatePart]]
        ) => row +: remaining
    )) ~ optional(pipe) ~ whitespace ~> (
        (elems: Seq[Seq[TemplatePart]]) => elems.map(_.toList).toList
    )
  }

  def showBorder: Rule1[Border] = rule { oneOrMore("-") ~> (() => ShowBorder) }
  def hideBorder: Rule1[Border] = rule { oneOrMore("=") ~> (() => HideBorder) }

  def leftAlignment: Rule1[(Alignment, Border)] = rule {
    optional(":") ~ (showBorder | hideBorder) ~> (
        (border: Border) => LeftAlignment -> border
    )
  }

  def rightAlignment: Rule1[(Alignment, Border)] = rule {
    (showBorder | hideBorder) ~ ":" ~> (
        (border: Border) => RightAlignment -> border
    )
  }

  def centerAlignment: Rule1[(Alignment, Border)] = rule {
    ":" ~ (showBorder | hideBorder) ~ ":" ~> (
        (border: Border) => CenterAlignment -> border
    )
  }

  def tableHeaderBreakString: Rule1[(Alignment, Border)] = rule {
    whitespace ~ (rightAlignment | centerAlignment | leftAlignment) ~ whitespace ~> (
        (alignment: (Alignment, Border)) => alignment
    )
  }

  def headerAnnotationPart: Rule1[HeaderAnnotation] = rule {
    openCloseAnnotationHeader ~ headerAnnotationContent ~ openCloseAnnotationHeader ~> (
        content => HeaderAnnotation(content)
    )
  }

  def noteAnnotationPart: Rule1[NoteAnnotation] = rule {
    openCloseAnnotationNote ~ noteAnnotationContent ~ openCloseAnnotationNote ~> (
        content => NoteAnnotation(content)
    )
  }

  def headerAnnotationContent: Rule1[String] = rule {
    capture(zeroOrMore(headerAnnotationContentChar))
  }

  def headerAnnotationContentChar: Rule0 = rule {
    !openCloseAnnotationHeader ~ ANY
  }

  def noteAnnotationContent: Rule1[String] = rule {
    capture(zeroOrMore(noteAnnotationContentChar))
  }

  def noteAnnotationContentChar: Rule0 = rule {
    !openCloseAnnotationNote ~ ANY
  }
}

final case class VariableSection(
    name: String,
    variables: List[VariableDefinition]
) extends TemplatePart
final case class SectionDefinition(name: String, parameters: Option[Parameters])
