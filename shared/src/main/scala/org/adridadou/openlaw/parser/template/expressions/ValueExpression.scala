package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.VariableType

case class ValueExpression(left:Expression, right:Expression, operation:ValueOperation) extends BinaryExpression {
  override def expressionType(executionResult: TemplateExecutionResult): VariableType = {
    val leftType = left.expressionType(executionResult)
    val rightType = right.expressionType(executionResult)

    leftType.operationWith(rightType, operation)
  }

  override def evaluate(executionResult: TemplateExecutionResult): Option[Any] = {
    operation match {
      case Plus => left.plus(right, executionResult)
      case Minus => left.minus(right, executionResult)
      case Multiple => left.multiply(right, executionResult)
      case Divide => left.divide(right, executionResult)
      case _ => None //TODO: make comparison evaluable
    }
  }

  override def validate(executionResult: TemplateExecutionResult): Option[String] = {
    val leftType = left.expressionType(executionResult)
    val rightType = right.expressionType(executionResult)
    if(!leftType.isCompatibleType(rightType, operation)) {
      Some("left and right expression are of incompatible types." + leftType.name + " & " + rightType.name + " in " + left.toString + " & " + right.toString)
    } else {
      leftType.validateOperation(this, executionResult) match {
        case Some(err) => Some(err)
        case None =>
          (for {
          _ <- left.missingInput(executionResult)
          _ <- right.missingInput(executionResult)
        } yield Unit ) match {
          case Left(ex) => Some(ex)
          case Right(_) => None
        }
      }
    }
  }

  override def toString:String = left.toString + operation.toString + right.toString
}