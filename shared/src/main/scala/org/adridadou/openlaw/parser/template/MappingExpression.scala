package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.VariableType
import org.adridadou.openlaw.result.{Result, Success}

final case class MappingExpression(expression: Expression, scopeExecutionResult: TemplateExecutionResult) extends Expression {
  override def missingInput(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    expression.missingInput(scopeExecutionResult)

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] =
    expression.validate(scopeExecutionResult)


  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] =
    expression.expressionType(scopeExecutionResult)

  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    expression.evaluate(scopeExecutionResult)

  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    expression.variables(scopeExecutionResult)
}
