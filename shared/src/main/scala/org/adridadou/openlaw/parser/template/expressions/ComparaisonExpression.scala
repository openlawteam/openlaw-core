package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import cats.implicits._
import org.adridadou.openlaw.OpenlawBoolean
import org.adridadou.openlaw.result.{Failure, Result, Success}

case class ComparaisonExpression(left:Expression, right:Expression, op:Operation) extends BinaryExpression {

  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawBoolean]] = {
    val x = for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
    } yield (leftValue, rightValue) match {
      case (one: Comparable[Any] @unchecked, two: Comparable[Any] @unchecked) => Success(one.compareTo(two))
      case _ => Failure(s"can not compare ${leftValue.getClass} and ${rightValue.getClass}")
    }

    x.flatten.map { comparaisonResult =>
      op match {
        case GreaterThan => comparaisonResult > 0
        case LesserThan => comparaisonResult < 0
        case GreaterOrEqual => comparaisonResult >= 0
        case LesserOrEqual => comparaisonResult <= 0
        case Equals => comparaisonResult === 0
      }
    }.map(Some(_))
  }

  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] = Success(YesNoType)

  override def toString: String = s"$left $op $right"
}
