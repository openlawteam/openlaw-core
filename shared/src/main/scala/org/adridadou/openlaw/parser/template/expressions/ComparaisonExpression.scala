package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import cats.implicits._
import org.adridadou.openlaw.OpenlawBoolean
import org.adridadou.openlaw.result.Result

case class ComparaisonExpression(left:Expression, right:Expression, op:Operation) extends BinaryExpression {

  override def evaluate(executionResult: TemplateExecutionResult): Option[OpenlawBoolean] = {
    val x = for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
    } yield (leftValue, rightValue) match {
      case (one: Comparable[Any] @unchecked, two: Comparable[Any] @unchecked) => one.compareTo(two)
      case _ => throw new RuntimeException(s"can not compare ${leftValue.getClass} and ${rightValue.getClass}")
    }

    x.map(comparaisonResult => {
      op match {
        case GreaterThan => comparaisonResult > 0
        case LesserThan => comparaisonResult < 0
        case GreaterOrEqual => comparaisonResult >= 0
        case LesserOrEqual => comparaisonResult <= 0
        case Equals => comparaisonResult === 0
      }
    })
  }

  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] = YesNoType

  override def toString: String = s"$left $op $right"
}
