package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.VariableType
import org.adridadou.openlaw.result.{Failure, Result, Success}

case class ValueExpression(left:Expression, right:Expression, operation:ValueOperation) extends BinaryExpression {
  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] = {
    for {
      leftType <- left.expressionType(executionResult)
      rightType <- right.expressionType(executionResult)
    } yield leftType.operationWith(rightType, operation)
  }

  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    operation match {
      case Plus => left.plus(right, executionResult)
      case Minus => left.minus(right, executionResult)
      case Multiple => left.multiply(right, executionResult)
      case Divide => left.divide(right, executionResult)
      case _ => Success(None) //TODO: make comparison evaluable
    }
  }

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] =
    for {
      leftType <- left.expressionType(executionResult)
      rightType <- right.expressionType(executionResult)
    } yield {
      if(!leftType.isCompatibleType(rightType, operation)) {
        Failure("left and right expression are of incompatible types." + leftType.name + " & " + rightType.name + " in " + left.toString + " & " + right.toString)
      } else {
        leftType.validateOperation(this, executionResult) match {
          case Some(err) => Failure(err)
          case None =>
            (for {
            _ <- left.missingInput(executionResult)
            _ <- right.missingInput(executionResult)
          } yield () ).left.flatMap(Failure(_))
        }
      }
    }

  override def toString:String = left.toString + operation.toString + right.toString
}