package org.adridadou.openlaw.parser.template.variableTypes

import VariableType._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw.{OpenlawBigDecimal, OpenlawValue}
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

case object NumberType extends VariableType("Number") {
  override def cast(value: String, executionResult: TemplateExecutionResult): OpenlawBigDecimal = BigDecimal(value)

  override def construct(constructorParams: Parameter,executionResult: TemplateExecutionResult): Result[Option[OpenlawBigDecimal]] = constructorParams match {
    case OneValueParameter(expr) =>
      val constructorType = expr.expressionType(executionResult)
      if(constructorType =!= this) {
        Failure(s"the constructor type should be $name but is ${constructorType.name}")
      } else {
        attempt(expr.evaluateT[OpenlawBigDecimal](executionResult))
      }
    case _ =>
      Failure(s"the constructor for $name only handles single values")
  }

  override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[OpenlawBigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[OpenlawBigDecimal](leftValue).underlying + convert[OpenlawBigDecimal](rightValue).underlying

  override def minus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[OpenlawBigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[OpenlawBigDecimal](leftValue).underlying - convert[OpenlawBigDecimal](rightValue).underlying

  override def multiply(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[OpenlawBigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[OpenlawBigDecimal](leftValue).underlying * convert[OpenlawBigDecimal](rightValue).underlying

  override def divide(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[OpenlawBigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight if convert[OpenlawBigDecimal](rightValue).underlying =!= BigDecimal(0)) yield convert[OpenlawBigDecimal](leftValue).underlying / convert[OpenlawBigDecimal](rightValue).underlying

  override def internalFormat(value: OpenlawValue): String =
    convert[OpenlawBigDecimal](value).underlying.toString

  override def getFormatter(formatter: FormatterDefinition, executionResult: TemplateExecutionResult):Formatter = formatter.name.toLowerCase match {
    case "notrailingzeros" => NoTrailingZerosFormatter
    case "raw" => RawNumberFormatter
    case "rounding" => formatter.parameters match {
      case Some(param:OneValueParameter) if param.expr.expressionType(executionResult) === NumberType => Rounding(param.expr)
      case Some(param:OneValueParameter)  => throw new RuntimeException(s"Rounding formatting only accepts expressions of type Number. The expression type is ${param.expr.expressionType(executionResult)}")
      case Some(_) => throw new RuntimeException("Rounding formatting only accepts a single parameter")
      case None => throw new RuntimeException("missing parameter for rounding")
    }
    case _ => defaultFormatter
  }

  override def defaultFormatter: Formatter = NoTrailingZerosFormatter


  override def validateOperation(expr: ValueExpression, executionResult: TemplateExecutionResult): Option[String] = {
    expr.operation match {
      case Divide =>
        expr.right.evaluate(executionResult) match {
          case Some(value:OpenlawBigDecimal) if value.underlying === BigDecimal(0) => Some(s"error while evaluating the expression '$expr': division by zero!")
          case _ => None
        }
      case _ =>
        None
    }
  }

  override def getTypeClass: Class[OpenlawBigDecimal] = classOf[OpenlawBigDecimal]

  def thisType: VariableType = NumberType
}

trait NumberFormatter {
  def formatNumber(bd:BigDecimal):String = {
    bd.bigDecimal.toPlainString.replaceAll("(\\d)(?=(\\d{3})+$)", "$1,")
  }
}

case object NoTrailingZerosFormatter extends Formatter with NumberFormatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    attempt(VariableType.convert[OpenlawBigDecimal](value).underlying.bigDecimal.stripTrailingZeros()) map {
      case bd => Seq(FreeText(Text(formatNumber(bd))))
    }
}

case object RawNumberFormatter extends Formatter with NumberFormatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    attempt(VariableType.convert[OpenlawBigDecimal](value).underlying.bigDecimal.stripTrailingZeros().toPlainString) map {
      case str => Seq(FreeText(Text(str)))
    }
}

case class Rounding(expr:Expression) extends Formatter with NumberFormatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = {
    expr.evaluate(executionResult)
      .map(VariableType.convert[OpenlawBigDecimal])
      .map(_.underlying.toInt).map(rounding => attempt(VariableType.convert[OpenlawBigDecimal](value).setScale(rounding, RoundingMode.HALF_UP))) match {
        case None => Success(Seq(FreeText(Text(value.toString))))
        case Some(Success(result)) => Success(Seq(FreeText(Text(result.toString))))
        case Some(Failure(e, message)) => Failure(e, message)
      }
  }
}