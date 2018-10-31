package org.adridadou.openlaw.parser.template

import org.parboiled2._

/**
  * Created by davidroon on 05.06.16.
  */
class MarkdownParser(val input: ParserInput) extends Parser with MarkdownRules {

  def rootRule: Rule1[Seq[TextElement]] = rule { zeroOrMore(loosenTextElement) ~ EOI ~>((elems:Seq[Seq[TextElement]]) => elems.flatten)}
}
