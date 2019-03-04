package org.adridadou.openlaw.parser.template.expressions

import cats.implicits._
import org.adridadou.openlaw.parser.template.{Compare, TemplateExecutionResult, VariableName}
import org.adridadou.openlaw.result.{Failure, Result, ResultNel, Success}
import org.adridadou.openlaw.result.Implicits.RichResultNel

trait BinaryExpression extends Expression {

  def left:Expression
  def right:Expression

  override def missingInput(executionResult: TemplateExecutionResult): ResultNel[Unit] =
    left.missingInput(executionResult) |+| right.missingInput(executionResult)

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] = {
    val leftType = left.expressionType(executionResult)
    val rightType = right.expressionType(executionResult)
    if(!leftType.isCompatibleType(rightType, Compare)) {
      Failure("left and right expression need to be of the same type to be computed." + leftType.name + " & " + rightType.name + " in " + left.toString + " & " + right.toString)
    } else {
      (left.missingInput(executionResult) |+| right.missingInput(executionResult)).toResult
    }
  }

  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    for {
      leftVar <- left.variables(executionResult)
      rightVar <- right.variables(executionResult)
    } yield leftVar ++ rightVar
}
