package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import cats.implicits._
import org.adridadou.openlaw.OpenlawBoolean
import org.adridadou.openlaw.result.{Failure, Result, Success}

final case class BooleanExpression(left:Expression, right:Expression, op:BooleanOperation) extends Expression {
  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawBoolean]] = {
    (for {
      leftOpt <- left.evaluate(executionResult)
      rightOpt <- right.evaluate(executionResult)
    } yield {
      (for {
        leftValue <- leftOpt
        rightValue <- rightOpt
      } yield {
        for {
          leftBoolean <- VariableType.convert[OpenlawBoolean](leftValue)
          rightBoolean <- VariableType.convert[OpenlawBoolean](rightValue)
        } yield {
          op match {
            case And => OpenlawBoolean(leftBoolean && rightBoolean)
            case Or => OpenlawBoolean(leftBoolean || rightBoolean)
          }
        }
      }).sequence
    }).flatten
  }

  override def expressionType(executionResult: TemplateExecutionResult):Result[VariableType] = Success(YesNoType)

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] =
    for {
      leftType <- left.expressionType(executionResult)
      rightType <- right.expressionType(executionResult)
			_ <- validate(leftType, rightType)
    } yield ()

	private def validate(leftType:VariableType, rightType:VariableType):Result[Unit] = {
		(leftType, rightType) match {
			case (l, _) if l =!= YesNoType =>
				Failure("The left part of the expression has to be a Boolean but instead is " + l.name + ":" + left.toString)
			case (_, r) if r =!= YesNoType =>
				Failure("The right part of the expression has to be a Boolean but instead is " + r.name + ":" + right.toString)
			case _ =>
				Success(())
		}
	}

  override def variables(executionResult:TemplateExecutionResult): Result[List[VariableName]] =
    left.evaluate(executionResult).flatMap { value =>
      op match {
        case And =>
          value match {
            case Some(OpenlawBoolean(true)) =>
              for {
                leftVars <- left.variables(executionResult)
                rightVars <- right.variables(executionResult)
              } yield leftVars ++ rightVars
            case _ => left.variables(executionResult)
          }
        case Or =>
          value match {
            case Some(OpenlawBoolean(false)) =>
              for {
                leftVars <- left.variables(executionResult)
                rightVars <- right.variables(executionResult)
              } yield leftVars ++ rightVars
            case _ => left.variables(executionResult)
          }
      }
    }

  override def toString: String = s"$left $op $right"

  override def missingInput(executionResult: TemplateExecutionResult): Result[List[VariableName]] = for {
      leftMissing <- left.missingInput(executionResult)
      rightMissing <- right.missingInput(executionResult)
    } yield leftMissing ++ rightMissing

}

final case class BooleanUnaryExpression(expr:Expression, op:BooleanUnaryOperation) extends Expression {
  override def expressionType(executionResult:TemplateExecutionResult): Result[VariableType] = Success(YesNoType)

  override def evaluate(executionResult:TemplateExecutionResult): Result[Option[OpenlawBoolean]] =
    expr
      .evaluate(executionResult)
      .flatMap { option =>
        option.map {
          case OpenlawBoolean(result) => op match {
            case Not => Success(OpenlawBoolean(!result))
            case _ => Failure(s"unknown symbol $op")
          }
          case other => Failure(s"bad type. Expected Boolean but got ${other.getClass.getSimpleName}:${expr.toString}")
        }.sequence
      }

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] = {
    expr.expressionType(executionResult).flatMap { exprType =>
      if (exprType =!= YesNoType) {
        Failure("The expression needs to be a Boolean but instead is " + exprType.name + ":" + expr.toString)
      } else {
        Success(())
      }
    }
  }

  override def variables(executionResult:TemplateExecutionResult): Result[List[VariableName]] = expr.variables(executionResult)

  override def missingInput(executionResult:TemplateExecutionResult): Result[List[VariableName]] =
    expr.missingInput(executionResult)

  override def toString: String = op.toString(expr)
}
