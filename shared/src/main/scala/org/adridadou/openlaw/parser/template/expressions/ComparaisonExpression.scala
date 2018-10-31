package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import cats.implicits._

case class ComparaisonExpression(left:Expression, right:Expression, op:Operation) extends BinaryExpression {

  override def evaluate(executionResult: TemplateExecutionResult): Option[Boolean] = {
    (for {leftValue <- left.evaluate(executionResult)
         rightValue <- right.evaluate(executionResult)
    } yield VariableType.convert[Comparable[Any]](leftValue).compareTo(rightValue)).map(comparaisonResult => {
      op match {
        case GreaterThan => comparaisonResult > 0
        case LesserThan => comparaisonResult < 0
        case GreaterOrEqual => comparaisonResult >= 0
        case LesserOrEqual => comparaisonResult <= 0
        case Equals => comparaisonResult === 0
      }
    })
  }

  override def expressionType(executionResult: TemplateExecutionResult): VariableType = YesNoType

  override def toString: String = s"$left $op $right"
}
