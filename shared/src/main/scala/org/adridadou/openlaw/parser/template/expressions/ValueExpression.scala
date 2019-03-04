package org.adridadou.openlaw.parser.template.expressions

import cats.implicits._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.VariableType
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.result.Implicits.RichResultNel

case class ValueExpression(left:Expression, right:Expression, operation:ValueOperation) extends BinaryExpression {
  override def expressionType(executionResult: TemplateExecutionResult): VariableType = {
    val leftType = left.expressionType(executionResult)
    val rightType = right.expressionType(executionResult)

    leftType.operationWith(rightType, operation)
  }

  override def evaluate(executionResult: TemplateExecutionResult): Option[Result[Any]] = {
    operation match {
      case Plus => left.plus(right, executionResult)
      case Minus => left.minus(right, executionResult)
      case Multiple => left.multiply(right, executionResult)
      case Divide => left.divide(right, executionResult)
      case _ => None //TODO: make comparison evaluable
    }
  }

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] = {
    val leftType = left.expressionType(executionResult)
    val rightType = right.expressionType(executionResult)
    if(!leftType.isCompatibleType(rightType, operation)) {
      Failure("left and right expression are of incompatible types." + leftType.name + " & " + rightType.name + " in " + left.toString + " & " + right.toString)
    } else {
      leftType.validateOperation(this, executionResult).flatMap { _ =>
        (left.missingInput(executionResult) |+| right.missingInput(executionResult)).toResult
      }
    }
  }

  override def toString:String = left.toString + operation.toString + right.toString
}