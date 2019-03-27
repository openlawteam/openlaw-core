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

  def blockRule:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | pageBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | headerAnnotationPart | noteAnnotationPart | textPart) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockInConditionalRule:Rule1[Block] = rule { zeroOrMore( centeredLine | rightThreeQuartersLine | rightLine | pageBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | headerAnnotationPart | noteAnnotationPart | textPartNoColons) ~> ((s: Seq[TemplatePart]) => Block(s))}
  def blockInConditionalElseRule:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | pageBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | headerAnnotationPart | noteAnnotationPart | textPartNoColons) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockNoStrong:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | headerAnnotationPart | noteAnnotationPart | textPartNoStrong) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockNoEm:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | headerAnnotationPart | noteAnnotationPart | textPartNoEm) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockNoStrongNoEm:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | headerAnnotationPart | noteAnnotationPart | textPartNoStrongNoEm) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def conditionalBlockSetKey:Rule1[ConditionalBlockSet]= rule { openB ~ oneOrMore(ws ~ conditionalBlockKey ~ ws) ~ closeB ~> ((blocks:Seq[ConditionalBlock]) => ConditionalBlockSet(blocks)) }

  def foreachBlockKey:Rule1[ForEachBlock]= rule { &(openB) ~ foreachBlock }

  def foreachBlock:Rule1[ForEachBlock] = rule { openB ~ "#for each" ~ variableName ~ ws ~ ":" ~ ws  ~ ExpressionRule ~ ws ~ "=>" ~ ws ~ blockRule ~ closeB ~> ((variable:VariableName, expression:Expression, block: Block) =>
    ForEachBlock(variable, expression, block))
  }

  def clauseBlockKey:Rule1[ClauseBlock]= rule { &(openB) ~ clauseBlock }

  def clauseBlock:Rule1[ClauseBlock] = rule { openB ~ "Clause:" ~ zeroOrMore(variableName) ~ ws ~ zeroOrMore(ExpressionRule) ~ ws ~ zeroOrMore(blockRule) ~ closeB ~> ((variable:VariableName, expression:Seq[Expression], block: Seq[Block]) =>
    ClauseBlock(variable, expression, block))
  }

  def conditionalBlockKey:Rule1[ConditionalBlock]= rule { &(openB) ~ conditionalBlock }

  def conditionalBlock:Rule1[ConditionalBlock] = rule { openB ~ ws  ~ conditionalExpressionRule ~ optional(ws ~ "=>") ~ ws ~ blockInConditionalRule ~ optional(conditionalBlockElse) ~ closeB ~> ((expression:Expression, block: Block, elseBlock:Option[Block]) => ConditionalBlock(block, elseBlock, expression))}

  def conditionalExpressionRule:Rule1[Expression] = rule {
    ExpressionRule ~>((expr:Expression) => expr match {
      case variable:VariableDefinition => variable.copy(variableTypeDefinition = Some(VariableTypeDefinition(YesNoType.name)))
      case name:VariableName => VariableDefinition(name, Some(VariableTypeDefinition(YesNoType.name)), None)
      case _ => expr
    })
  }

  def conditionalBlockElse:Rule1[Block] = rule { "::" ~ ws ~ blockInConditionalElseRule}

  def codeBlockKey:Rule1[CodeBlock] = rule {&(openA) ~ codeBlock}

  def codeBlock:Rule1[CodeBlock] = rule {openA ~  zeroOrMore( ws ~ (varAliasKey | varMemberKey | varKey | comment | variableSectionKey) ~ ws) ~ closeA ~> ((s:Seq[TemplatePart]) => CodeBlock(s))}

  def comment:Rule1[EmptyTemplatePart.type ] = rule { &("#" | "//") ~ capture(commentsChar) ~ "\n" ~>((s:String) => EmptyTemplatePart )}

  def sectionKey:Rule1[Section] = rule {
    &(sectionChar) ~ section
  }

  def section:Rule1[Section] = rule {
    capture(oneOrMore("^")) ~ optional(sectionDefinition) ~> ((elems:String, namedSection:Option[SectionDefinition]) => Section(UUID.randomUUID().toString, namedSection, elems.length))
  }

  def sectionDefinition:Rule1[SectionDefinition] = rule {
    "(" ~ charsKeyAST ~ optional("(" ~ parametersMapDefinition ~ ")") ~ ")" ~> ((name:String, params:Option[Parameters]) => SectionDefinition(name, params))
  }

  def variableSectionKey:Rule1[VariableSection] = rule {
    &(variableSectionChar) ~ variableSection
  }

  def variableSection:Rule1[VariableSection] = rule {
    variableSectionChar ~ capture(characters) ~ variableSectionChar ~ ws ~ oneOrMore(wsNoReturn ~ variable ~ wsNoReturn).separatedBy("\n") ~> ((name:String, variables:Seq[VariableDefinition]) => VariableSection(name, variables))
  }

  def centeredLine: Rule1[TemplateText] = rule {
    capture(centered) ~> ((_: String) => TemplateText(Seq(Centered)))
  }

  def rightLine: Rule1[TemplateText] = rule {
    capture(right) ~> ((_: String) => TemplateText(Seq(RightAlign)))
  }

  def rightThreeQuartersLine: Rule1[TemplateText] = rule {
    capture(rightThreeQuarters) ~> ((_: String) => TemplateText(Seq(RightThreeQuarters)))
  }

  def pageBreak: Rule1[TemplateText] = rule {
    capture(pagebreak) ~ "\n" ~> ((_: String) => TemplateText(Seq(PageBreak)))
  }

  def indentLine: Rule1[TemplateText] = rule {
    capture(indent) ~> ((_: String) => TemplateText(Seq(Indent)))
  }

  def textPart: Rule1[TemplateText] = rule {
    textElement ~> ((s: Seq[TemplatePart]) => TemplateText(s)) }

  def textPartNoColons: Rule1[TemplateText] = rule {
    textElementNoColons ~> ((s: Seq[TemplatePart]) => TemplateText(s)) }

  def textPartNoStrong: Rule1[TemplateText] = rule {
    textElementNoStrong ~> ((s: Seq[TemplatePart]) => TemplateText(s)) }

  def textPartNoEm: Rule1[TemplateText] = rule {
    textElementNoEm ~> ((s: Seq[TemplatePart]) => TemplateText(s)) }

  def textPartNoStrongNoEm: Rule1[TemplateText] = rule {
    textNoReturn ~> ((s: Seq[TemplatePart]) => TemplateText(s)) }

  def textElement: Rule1[Seq[TemplatePart]] = rule {
    tableKey | strongWord | emWord | text | pipeText | starText
  }

  def textElementNoStrong: Rule1[Seq[TemplatePart]] = rule {
    innerEmWord | textNoReturn | pipeText
  }

  def textElementNoColons: Rule1[Seq[TemplatePart]] = rule {
    tableKey| strongWord | emWord | textNoColons | pipeText | starText
  }

  def textElementNoEm: Rule1[Seq[TemplatePart]] = rule {
    innerStrongWord | textNoReturn | pipeText
  }

  def twoStar: Rule0 = rule(strong)
  def twoStarcontents: Rule1[Block] = rule { !twoStar ~ blockNoStrong }
  def innerTwoStarcontents: Rule1[Block] = rule { !twoStar ~ blockNoStrongNoEm }

  def strongWord: Rule1[Seq[TemplatePart]] = rule { twoStar ~ twoStarcontents ~ twoStar ~> ((block: Block) => Seq(Strong) ++ block.elems ++ Seq(Strong)) }
  def innerStrongWord: Rule1[Seq[TemplatePart]] = rule { twoStar ~ innerTwoStarcontents ~ twoStar ~> ((block: Block) => Seq(Strong) ++ block.elems ++ Seq(Strong)) }

  def oneStar: Rule0 = rule(em)
  def oneStarcontents: Rule1[Seq[TemplatePart]] = rule { !oneStar ~ blockNoEm ~> ((block: Block) => block.elems) }
  def innerOneStarcontents: Rule1[Seq[TemplatePart]] = rule { !oneStar ~ blockNoStrongNoEm ~> ((block: Block) => block.elems) }

  def emWord: Rule1[Seq[TemplatePart]] = rule { oneStar ~ oneStarcontents ~ oneStar ~> ((elems: Seq[TemplatePart]) => Seq(Em) ++ elems ++ Seq(Em)) }
  def innerEmWord: Rule1[Seq[TemplatePart]] = rule { oneStar ~ innerOneStarcontents ~ oneStar ~> ((elems: Seq[TemplatePart]) => Seq(Em) ++ elems ++ Seq(Em)) }

  def text: Rule1[Seq[TemplatePart]] = rule {
    capture(characters) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

  def textNoReturn: Rule1[Seq[TemplatePart]] = rule {
    capture(charactersNoReturn) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

  def textNoColons: Rule1[Seq[TemplatePart]] = rule {
    capture(charactersNoColons) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

  def starText: Rule1[Seq[TemplatePart]] = rule {
    capture(em) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

  def pipeText: Rule1[Seq[TemplatePart]] = rule {
    capture(pipe) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

  def tableText: Rule1[TemplatePart] = rule {
    capture(noneOf(pipe + nl)) ~> ((s: String) => Text(s))
  }

  // the table parsing construct below may return empty whitespace at the end of the cell, this trims it and accumulates text nodes
  @tailrec final def accumulateTextAndTrim(seq: Seq[TemplatePart], accu: Seq[TemplatePart] = Seq()): Seq[TemplatePart] = seq match {
    case Seq() => accu
    case seq @ Seq(head, tail @ _*) =>
      val texts = seq.takeWhile(_.isInstanceOf[Text]).asInstanceOf[Seq[Text]]
      val trimmed = texts.map(_.str).foldLeft("")(_ + _).trim
      texts.size match {
        case 0 => accumulateTextAndTrim(tail, accu :+ head)
        case _ if trimmed === "" => accumulateTextAndTrim(seq.drop(texts.size), accu)
        case x => accumulateTextAndTrim(seq.drop(x), accu :+ Text(trimmed))
      }
  }

  def whitespace:Rule0 = rule { zeroOrMore(anyOf(tabOrSpace)) }
  def tableColumnEntryBlock: Rule1[Seq[TemplatePart]] = rule { oneOrMore(varAliasKey | varKey | varMemberKey | conditionalBlockSetKey | conditionalBlockKey | foreachBlockKey | tableText) ~> ((seq: Seq[TemplatePart]) => accumulateTextAndTrim(seq))  }

  def tableKey: Rule1[Seq[Table]] = rule {
    &(pipe) ~ table
  }

  def table: Rule1[Seq[Table]] = rule {
    tableWithHeaderAndRow | tableWithoutHeader | tableWithoutRow
  }

  def tableWithHeaderAndRow:Rule1[Seq[Table]] = rule {
    tableHeader ~ nl ~ whitespace ~ oneOrMore(tableRow ~ (nl | EOI)) ~> ((headers: Seq[Seq[TemplatePart]], rows: Seq[Seq[Seq[TemplatePart]]]) => Seq(Table(headers.map(_.toList).toList, rows.map(_.map(_.toList).toList).toList)))
  }

  def tableWithoutHeader:Rule1[Seq[Table]] = rule {
    oneOrMore(tableRow ~ whitespace ~ (nl | EOI)) ~ whitespace ~> ((rows: Seq[Seq[Seq[TemplatePart]]]) => Seq(Table(List(), rows.map(_.map(_.toList).toList).toList)))
  }

  def tableWithoutRow:Rule1[Seq[Table]] = rule {
    tableHeader ~ nl ~> ((headers: Seq[Seq[TemplatePart]]) => Seq(Table(headers.map(_.toList).toList, List())))
  }

  def tableHeader: Rule1[Seq[Seq[TemplatePart]]] = rule { tableRow ~ nl ~ whitespace ~ tableHeaderBreak ~ whitespace }
  def tableHeaderBreak: Rule0 = rule { whitespace ~ pipe ~ oneOrMore(tableHeaderBreakString).separatedBy(pipe) ~ whitespace ~ pipe ~ whitespace }

  def tableColumnEntry: Rule1[Seq[TemplatePart]] = rule { whitespace ~ tableColumnEntryBlock ~ whitespace }

  def tableRow: Rule1[Seq[Seq[TemplatePart]]] = rule { whitespace ~ pipe ~ (tableColumnEntry ~ pipe ~ oneOrMore(tableColumnEntry).separatedBy(pipe) ~ pipe ~ whitespace ~> ((row: Seq[TemplatePart], remaining: Seq[Seq[TemplatePart]]) => row +: remaining)) ~ optional(pipe) ~ whitespace}

  def tableHeaderBreakString: Rule0 = rule {
    whitespace ~ (oneOrMore("-") ~ ":" |
      whitespace ~ oneOrMore("-") ~ whitespace |
      ":" ~ oneOrMore("-") ~ ":" |
      ":" ~ oneOrMore("-")) ~ whitespace
  }

  def headerAnnotationPart: Rule1[HeaderAnnotation] = rule {
    openCloseAnnotationHeader ~ headerAnnotationContent ~ openCloseAnnotationHeader ~> (content => HeaderAnnotation(content))
  }

  def noteAnnotationPart: Rule1[NoteAnnotation] = rule {
    openCloseAnnotationNote ~ noteAnnotationContent ~ openCloseAnnotationNote ~> (content => NoteAnnotation(content))
  }

  def headerAnnotationContent:Rule1[String] = rule {
    capture(zeroOrMore(headerAnnotationContentChar))
  }

  def headerAnnotationContentChar:Rule0 = rule {
    !openCloseAnnotationHeader ~  ANY
  }

  def noteAnnotationContent:Rule1[String] = rule {
    capture(zeroOrMore(noteAnnotationContentChar))
  }

  def noteAnnotationContentChar:Rule0 = rule {
    !openCloseAnnotationNote ~  ANY
  }
}

case class VariableSection(name:String, variables:Seq[VariableDefinition]) extends TemplatePart
case class SectionDefinition(name:String, parameters:Option[Parameters])
case class HeaderAnnotation(content:String) extends TemplatePart with AgreementElement
case class NoteAnnotation(content:String) extends TemplatePart with AgreementElement
