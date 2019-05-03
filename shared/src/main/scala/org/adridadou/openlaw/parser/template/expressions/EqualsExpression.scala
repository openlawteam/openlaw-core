package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.BooleanOpenlawValue
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}

case class EqualsExpression(left:Expression, right:Expression) extends BinaryExpression {

  override def evaluate(executionResult: TemplateExecutionResult): Option[BooleanOpenlawValue] = {
    for{ leftValue <- left.evaluate(executionResult)
        rightValue <- right.evaluate(executionResult)
    } yield leftValue == rightValue
  }

  override def expressionType(executionResult: TemplateExecutionResult): VariableType = YesNoType
}
