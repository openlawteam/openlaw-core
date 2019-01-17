package org.adridadou.openlaw.parser.template

import java.util.UUID

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.YesNoType
import org.parboiled2.{Parser, Rule0, Rule1}

/**
  * Created by davidroon on 06.06.17.
  */
trait BlockRules extends Parser with ExpressionRules with GlobalRules {

  def blockRule:Rule1[Block] =              rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | pageBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | topAnnotationPart | middleAnnotationPart | textPart) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockInConditionalRule:Rule1[Block] = rule { zeroOrMore( centeredLine | rightThreeQuartersLine | rightLine | pageBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | topAnnotationPart | middleAnnotationPart | textPartNoColons) ~> ((s: Seq[TemplatePart]) => Block(s))}
  def blockInConditionalElseRule:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | pageBreak | indentLine | variableSectionKey | sectionKey | varAliasKey | varKey | varMemberKey | foreachBlockKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey | topAnnotationPart | middleAnnotationPart | textPartNoColons) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockNoStrong:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | topAnnotationPart | middleAnnotationPart | textPartNoStrong) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockNoEm:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | topAnnotationPart | middleAnnotationPart | textPartNoEm) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def blockNoStrongNoEm:Rule1[Block] = rule { zeroOrMore(centeredLine | rightThreeQuartersLine | rightLine | indentLine | varAliasKey | varKey | varMemberKey | topAnnotationPart | middleAnnotationPart | textPartNoStrongNoEm) ~> ((s: Seq[TemplatePart]) => Block(s))}

  def conditionalBlockSetKey:Rule1[ConditionalBlockSet]= rule { openB ~ oneOrMore(ws ~ conditionalBlockKey ~ ws) ~ closeB ~> ((blocks:Seq[ConditionalBlock]) => ConditionalBlockSet(blocks)) }

  def foreachBlockKey:Rule1[ForEachBlock]= rule { &(openB) ~ foreachBlock }

  def foreachBlock:Rule1[ForEachBlock] = rule { openB ~ "#for each" ~ variableName ~ ws ~ ":" ~ ws  ~ ExpressionRule ~ ws ~ "=>" ~ ws ~ blockRule ~ closeB ~> ((variable:VariableName, expression:Expression, block: Block) =>
    ForEachBlock(variable, expression, block))
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

  def conditionalBlockKeyElse:Rule1[Block]= rule { &("::") ~ conditionalBlockElse }

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
    table | strongWord | emWord | textWithStar
  }

  def textElementNoStrong: Rule1[Seq[TemplatePart]] = rule {
    innerEmWord | textNoReturn
  }

  def textElementNoColons: Rule1[Seq[TemplatePart]] = rule {
    table | strongWord | emWord | textNoColons
  }

  def textElementNoEm: Rule1[Seq[TemplatePart]] = rule {
    innerStrongWord | textNoReturn
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

  def textWithStar: Rule1[Seq[TemplatePart]] = rule {
    capture(em | pipe | characters) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

  def whitespace:Rule0 = rule { zeroOrMore(anyOf(tabOrSpace)) }
  def tableColumnEntryBlock: Rule1[Seq[TemplatePart]] = rule { oneOrMore(varAliasKey | varKey | varMemberKey | conditionalBlockSetKey | conditionalBlockKey | codeBlockKey) }
  def tableColumnEntryText: Rule1[Seq[TemplatePart]] = rule { capture(oneOrMore(noneOf(pipe + nl))) ~> ((s: String) => Seq(Text(TextCleaning.dots(s.trim)))) }

  def table: Rule1[Seq[Table]] = rule {
    tableHeader ~ nl ~ oneOrMore(tableRow ~ (nl | EOI)) ~> ((headers: Seq[Seq[TemplatePart]], rows: Seq[Seq[Seq[TemplatePart]]]) => Seq(Table(headers, rows)))
  }

  def tableHeader: Rule1[Seq[Seq[TemplatePart]]] = rule { tableRow ~ nl ~ tableHeaderBreak }
  def tableHeaderBreak: Rule0 = rule { whitespace ~ optional(pipe) ~ oneOrMore(tableHeaderBreakString).separatedBy(pipe) ~ optional(pipe) ~ whitespace }

  def tableColumnEntry: Rule1[Seq[TemplatePart]] = rule { whitespace ~ (tableColumnEntryBlock | tableColumnEntryText) ~ whitespace }
  def tableRow: Rule1[Seq[Seq[TemplatePart]]] = rule { whitespace ~ optional(pipe) ~ (tableColumnEntry ~ pipe ~ oneOrMore(tableColumnEntry).separatedBy(pipe) ~> ((row: Seq[TemplatePart], remaining: Seq[Seq[TemplatePart]]) => row +: remaining)) ~ optional(pipe) ~ whitespace}

  def tableHeaderBreakString: Rule0 = rule {
    whitespace ~ (oneOrMore("-") ~ ":" |
      whitespace ~ oneOrMore("-") ~ whitespace |
      ":" ~ oneOrMore("-") ~ ":" |
      ":" ~ oneOrMore("-")) ~ whitespace
  }

  def topAnnotationPart: Rule1[TopAnnotation] = rule {
    openCloseAnnotation ~ annotationContent ~ openCloseAnnotation ~> (content => TopAnnotation(content))
  }

  def middleAnnotationPart: Rule1[MiddleAnnotation] = rule {
    openCloseAnnotation ~ annotationContent ~ openCloseAnnotation ~> (content => MiddleAnnotation(content))
  }

  def annotationContent:Rule1[String] = rule {
    capture(zeroOrMore(annotationContentChar))
  }

  def annotationContentChar:Rule0 = rule {
    !openCloseAnnotation ~  ANY
  }
}

case class VariableSection(name:String, variables:Seq[VariableDefinition]) extends TemplatePart
case class SectionDefinition(name:String, parameters:Option[Parameters])
case class TopAnnotation(content:String) extends TemplatePart with AgreementElement
case class MiddleAnnotation(content:String) extends TemplatePart with AgreementElement

