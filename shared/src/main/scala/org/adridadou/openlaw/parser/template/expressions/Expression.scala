package org.adridadou.openlaw.parser.template.expressions

import io.circe._
import org.adridadou.openlaw.parser.template.variableTypes.VariableType
import org.adridadou.openlaw.parser.template._

trait Expression {
  def missingInput(executionResult: TemplateExecutionResult): Either[String, Seq[VariableName]]

  def validate(executionResult: TemplateExecutionResult): Option[String]

  def minus(right: Expression, executionResult: TemplateExecutionResult): Option[Any] = expressionType(executionResult).minus(evaluate(executionResult), right.evaluate(executionResult), executionResult)
  def plus(right: Expression, executionResult: TemplateExecutionResult): Option[Any] = expressionType(executionResult).plus(evaluate(executionResult), right.evaluate(executionResult), executionResult)
  def multiply(right: Expression, executionResult: TemplateExecutionResult): Option[Any] = expressionType(executionResult).multiply(evaluate(executionResult), right.evaluate(executionResult), executionResult)
  def divide(right: Expression, executionResult: TemplateExecutionResult): Option[Any] = expressionType(executionResult).divide(evaluate(executionResult), right.evaluate(executionResult), executionResult)

  def expressionType(executionResult: TemplateExecutionResult):VariableType
  def evaluate(executionResult: TemplateExecutionResult):Option[Any]
  def variables(executionResult: TemplateExecutionResult):Seq[VariableName]
}

case class ParensExpression(expr:Expression) extends Expression {
  override def missingInput(executionResult: TemplateExecutionResult): Either[String, Seq[VariableName]] =
    expr.missingInput(executionResult)

  override def validate(executionResult: TemplateExecutionResult): Option[String] =
    expr.validate(executionResult)

  override def expressionType(executionResult: TemplateExecutionResult): VariableType =
    expr.expressionType(executionResult)

  override def evaluate(executionResult: TemplateExecutionResult): Option[Any] =
    expr.evaluate(executionResult)

  override def variables(executionResult: TemplateExecutionResult): Seq[VariableName] =
    expr.variables(executionResult)

  override def toString: String = s"($expr)"
}


object Expression {
  private val exprParser = new ExpressionParserService()

  implicit val exprEnc:Encoder[Expression] = (a: Expression) => Json.fromString(a.toString)
  implicit val exprDec:Decoder[Expression] = (c: HCursor) => c.as[String].flatMap(parseExpression)

  private def parseExpression(value:String):Either[DecodingFailure, Expression] = exprParser.parseExpression(value) match {
    case Right(expr) => Right(expr)
    case Left(ex) => throw new RuntimeException(ex)
  }
}
