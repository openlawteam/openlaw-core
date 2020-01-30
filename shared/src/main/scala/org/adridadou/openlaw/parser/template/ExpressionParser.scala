package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.parboiled2._

/**
  * Created by davidroon on 05.06.16.
  */
class ExpressionParser(val input: ParserInput)
    extends Parser
    with ExpressionRules {
  def root: Rule1[Expression] = rule {
    ExpressionRule ~ EOI
  }
}
