package org.adridadou.openlaw.parser.template

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.variableTypes.VariableType

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.Result

object VariableAliasing {
  implicit val variableAliasingEnc: Encoder[VariableAliasing] = deriveEncoder[VariableAliasing]
  implicit val variableAliasingDec: Decoder[VariableAliasing] = deriveDecoder[VariableAliasing]
}

case class VariableAliasing(name:VariableName, expr:Expression) extends Expression with TemplatePart{
  def validate(executionResult: TemplateExecutionResult): Result[Unit] =
    expr.validate(executionResult)

  override def expressionType(executionResult: TemplateExecutionResult): VariableType =
    expr.expressionType(executionResult)

  override def evaluate(executionResult: TemplateExecutionResult): Option[Any] =
    expr.evaluate(executionResult)

  override def variables(executionResult: TemplateExecutionResult): Seq[VariableName] =
    expr.variables(executionResult)

  override def missingInput(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    expr.missingInput(executionResult)
}
