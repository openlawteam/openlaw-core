package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import cats.implicits._
import org.adridadou.openlaw.result.{Failure, Result, Success}

case class BooleanExpression(left:Expression, right:Expression, op:BooleanOperation) extends Expression {
  override def evaluate(executionResult: TemplateExecutionResult): Option[Boolean] = op match {
    case And => for{leftValue <- left.evaluate(executionResult)
                    rightValue <- right.evaluate(executionResult)
                } yield VariableType.convert[Boolean](leftValue) && VariableType.convert[Boolean](rightValue)

    case Or => for{leftValue <- left.evaluate(executionResult)
                   rightValue <- right.evaluate(executionResult)
                } yield VariableType.convert[Boolean](leftValue) || VariableType.convert[Boolean](rightValue)
  }

  override def expressionType(executionResult: TemplateExecutionResult):VariableType = YesNoType

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] = {
    (left.expressionType(executionResult), right.expressionType(executionResult)) match {
      case (leftType, _) if leftType =!= YesNoType => Failure("The left part of the expression has to be a Boolean but instead is " + leftType.name + ":" + left.toString)
      case (_, rightType) if rightType =!= YesNoType => Failure("The right part of the expression has to be a Boolean but instead is " + rightType.name + ":" + right.toString)
      case _ => Success(())
    }
  }

  override def variables(executionResult:TemplateExecutionResult): Seq[VariableName] = op match {
    case And =>
      left.evaluate(executionResult) match {
        case Some(true) => left.variables(executionResult) ++ right.variables(executionResult)
        case _ => left.variables(executionResult)
      }
    case Or =>
      left.evaluate(executionResult) match {
        case Some(false) => left.variables(executionResult) ++ right.variables(executionResult)
        case _ => left.variables(executionResult)
      }

  }

  override def toString: String = left.toString + op.toString + right.toString

  override def missingInput(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] = for {
      leftMissing <- left.missingInput(executionResult)
      rightMissing <- right.missingInput(executionResult)
    } yield leftMissing ++ rightMissing

}

case class BooleanUnaryExpression(expr:Expression, op:BooleanUnaryOperation) extends Expression {
  override def expressionType(executionResult:TemplateExecutionResult): VariableType = YesNoType

  override def evaluate(executionResult:TemplateExecutionResult): Option[Boolean] = expr.evaluate(executionResult).map({
    case result:Boolean => op match {
      case Not => !result
      case _ => throw new RuntimeException(s"unknown symbol $op")
    }
    case other => throw new RuntimeException(s"bad type. Expected Boolean but got ${other.getClass.getSimpleName}:${expr.toString}")
  })

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] = {
    val exprType = expr.expressionType(executionResult)
    if(exprType =!= YesNoType) {
      Failure("The expression needs to be a Boolean but instead is " + exprType.name + ":" + expr.toString)
    } else {
      Success(())
    }
  }

  override def variables(executionResult:TemplateExecutionResult): Seq[VariableName] = expr.variables(executionResult)

  override def missingInput(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] =
    expr.missingInput(executionResult)

  override def toString: String = op.toString(expr)
}
