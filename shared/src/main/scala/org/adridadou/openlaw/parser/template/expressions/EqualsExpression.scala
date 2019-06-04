package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.OpenlawBoolean
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import org.adridadou.openlaw.result.{Result, Success}

case class EqualsExpression(left:Expression, right:Expression) extends BinaryExpression {

  override def evaluate(executionResult: TemplateExecutionResult): Option[OpenlawBoolean] = {
    for{ leftValue <- left.evaluate(executionResult)
        rightValue <- right.evaluate(executionResult)
    } yield leftValue == rightValue
  }

  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] = Success(YesNoType)
}
