package org.adridadou.openlaw.parser.template.expressions

import cats.implicits._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import org.adridadou.openlaw.result.{Failure, Result, ResultNel, Success}
import shapeless.Succ

case class BooleanExpression(left:Expression, right:Expression, op:BooleanOperation) extends Expression {
  override def evaluate(executionResult: TemplateExecutionResult): Option[Result[Boolean]] = op match {
    case And =>
      for {
        leftValue <- left.evaluate(executionResult)
        rightValue <- right.evaluate(executionResult)
      } yield {
        for {
          left <- VariableType.convert[Boolean](leftValue)
          right <- VariableType.convert[Boolean](rightValue)
        } yield left && right
      }

    case Or =>
      for {
        leftValue <- left.evaluate(executionResult)
        rightValue <- right.evaluate(executionResult)
      } yield {
        for {
          left <- VariableType.convert[Boolean](leftValue)
          right <- VariableType.convert[Boolean](rightValue)
        } yield left || right
      }
  }

  override def expressionType(executionResult: TemplateExecutionResult):VariableType = YesNoType

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] = {
    (left.expressionType(executionResult), right.expressionType(executionResult)) match {
      case (leftType, _) if leftType =!= YesNoType => Failure("The left part of the expression has to be a Boolean but instead is " + leftType.name + ":" + left.toString)
      case (_, rightType) if rightType =!= YesNoType => Failure("The right part of the expression has to be a Boolean but instead is " + rightType.name + ":" + right.toString)
      case _ => Success(())
    }
  }

  override def variables(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] = op match {
    case And =>
      left
        .evaluate(executionResult)
        .sequence
        .flatMap { option =>
          option match {
            case Some(true) =>
              for {
                leftVar <- left.variables(executionResult)
                rightVar <- right.variables(executionResult)
              } yield leftVar ++ rightVar
            case _ => left.variables(executionResult)
          }
        }

    case Or =>
      left
        .evaluate(executionResult)
        .sequence
        .flatMap { option =>
          option match {
            case Some(false) =>
              for {
                leftVar <- left.variables(executionResult)
                rightVar <- right.variables(executionResult)
              } yield leftVar ++ rightVar
            case _ => left.variables(executionResult)
          }
        }
  }

  override def toString: String = left.toString + op.toString + right.toString

  override def missingInput(executionResult: TemplateExecutionResult): ResultNel[Unit] =
    left.missingInput(executionResult) |+| right.missingInput(executionResult)
}

case class BooleanUnaryExpression(expr:Expression, op:BooleanUnaryOperation) extends Expression {
  override def expressionType(executionResult:TemplateExecutionResult): VariableType = YesNoType

  override def evaluate(executionResult:TemplateExecutionResult): Option[Result[Boolean]] = expr.evaluate(executionResult).map { result =>
    result.flatMap {
      case r: Boolean => op match {
        case Not => Success(!r)
        case _ => Failure(s"unknown symbol $op")
      }
      case other => Failure(s"bad type. Expected Boolean but got ${other.getClass.getSimpleName}:${expr.toString}")
    }
  }

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] = {
    val exprType = expr.expressionType(executionResult)
    if(exprType =!= YesNoType) {
      Failure("The expression needs to be a Boolean but instead is " + exprType.name + ":" + expr.toString)
    } else {
      Success(())
    }
  }

  override def variables(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] =
    expr.variables(executionResult)

  override def missingInput(executionResult:TemplateExecutionResult): ResultNel[Unit] =
    expr.missingInput(executionResult)

  override def toString: String = op.toString(expr)
}
