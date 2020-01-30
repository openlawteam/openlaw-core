package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.OpenlawBoolean
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.parser.template.variableTypes.{
  VariableType,
  YesNoType
}
import org.adridadou.openlaw.result.{Result, Success}

final case class EqualsExpression(left: Expression, right: Expression)
    extends BinaryExpression {

  override def evaluate(
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawBoolean]] = {
    for {
      leftOption <- left.evaluate(executionResult)
      rightOption <- right.evaluate(executionResult)
    } yield for {
      leftValue <- leftOption
      rightValue <- rightOption
    } yield leftValue == rightValue
  }

  override def expressionType(
      executionResult: TemplateExecutionResult
  ): Result[VariableType] = Success(YesNoType)
}
