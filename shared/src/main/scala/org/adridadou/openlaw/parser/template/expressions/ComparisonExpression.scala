package org.adridadou.openlaw.parser.template.expressions

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, YesNoType}
import cats.implicits._
import org.adridadou.openlaw.result.Result

case class ComparisonExpression(left:Expression, right:Expression, op:Operation) extends BinaryExpression {

  override def evaluate(executionResult: TemplateExecutionResult): Option[Result[Boolean]] = {
    (for {leftValue <- left.evaluate(executionResult)
         rightValue <- right.evaluate(executionResult)
    } yield {
      VariableType.convert[Comparable[Any]](leftValue).map(_.compareTo(rightValue))
    }).map { comparisonResult =>
      comparisonResult.map { comparison =>
        op match {
          case GreaterThan => comparison > 0
          case LesserThan => comparison < 0
          case GreaterOrEqual => comparison >= 0
          case LesserOrEqual => comparison <= 0
          case Equals => comparison === 0
        }
      }
    }
  }

  override def expressionType(executionResult: TemplateExecutionResult): VariableType = YesNoType

  override def toString: String = s"$left $op $right"
}
