package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.parser.template.variableTypes.VariableType

import scala.util.{Failure, Success, Try}
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Result, ResultNel}

case class VariableAliasing(name:VariableName, expr:Expression) extends Expression with TemplatePart{
  def validate(executionResult: TemplateExecutionResult): Result[Unit] =
    expr.validate(executionResult)

  override def expressionType(executionResult: TemplateExecutionResult): VariableType =
    expr.expressionType(executionResult)

  override def evaluate(executionResult: TemplateExecutionResult): Option[Result[Any]] =
    expr.evaluate(executionResult)

  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    expr.variables(executionResult)

  override def missingInput(executionResult: TemplateExecutionResult): ResultNel[Unit] =
    expr.missingInput(executionResult)
}
