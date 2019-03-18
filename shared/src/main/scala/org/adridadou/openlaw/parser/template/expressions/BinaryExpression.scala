package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template.{Compare, TemplateExecutionResult, VariableName}
import org.adridadou.openlaw.result.{Failure, Result, Success}

trait BinaryExpression extends Expression {

  def left:Expression
  def right:Expression

  override def missingInput(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] = for {
      leftMissing <- left.missingInput(executionResult)
      rightMissing <- right.missingInput(executionResult)
    } yield leftMissing ++ rightMissing

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] = {
    val leftType = left.expressionType(executionResult)
    val rightType = right.expressionType(executionResult)
    if(!leftType.isCompatibleType(rightType, Compare)) {
      Failure("left and right expression need to be of the same type to be computed." + leftType.name + " & " + rightType.name + " in " + left.toString + " & " + right.toString)
    } else {
      (for {
        _ <- left.missingInput(executionResult)
        _ <- right.missingInput(executionResult)
      } yield Unit) match {
        case Left(ex) =>
          Failure(ex)
        case Right(_) =>
          Success(())
      }
    }
  }

  override def variables(executionResult: TemplateExecutionResult): Seq[VariableName] =
    left.variables(executionResult) ++ right.variables(executionResult)
}
