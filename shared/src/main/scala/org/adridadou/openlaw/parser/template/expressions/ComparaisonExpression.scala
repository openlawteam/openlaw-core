package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import cats.implicits._
import org.adridadou.openlaw.BooleanOpenlawValue

case class ComparaisonExpression(left:Expression, right:Expression, op:Operation) extends BinaryExpression {

  override def evaluate(executionResult: TemplateExecutionResult): Option[BooleanOpenlawValue] = {
    for {leftValue <- left.evaluate(executionResult)
         rightValue <- right.evaluate(executionResult)
    } yield {
      (leftValue.get, rightValue.get) match {
        case (l: Comparable[Any] @unchecked, r: Comparable[Any] @unchecked) =>
          val comparisonResult = l.compareTo(r)
          op match {
            case GreaterThan => comparisonResult > 0
            case LesserThan => comparisonResult < 0
            case GreaterOrEqual => comparisonResult >= 0
            case LesserOrEqual => comparisonResult <= 0
            case Equals => comparisonResult === 0
          }
        case _ =>
          false
      }
    }
  }

  override def expressionType(executionResult: TemplateExecutionResult): VariableType = YesNoType

  override def toString: String = s"$left $op $right"
}
