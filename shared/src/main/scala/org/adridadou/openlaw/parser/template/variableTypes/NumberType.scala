package org.adridadou.openlaw.parser.template.variableTypes

import VariableType._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw
import org.adridadou.openlaw.{BigDecimalOpenlawValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

case object NumberType extends VariableType("Number") {
  override def cast(value: String, executionResult: TemplateExecutionResult): BigDecimalOpenlawValue = BigDecimal(value)

  override def construct(constructorParams: Parameter,executionResult: TemplateExecutionResult): Result[Option[BigDecimalOpenlawValue]] = constructorParams match {
    case OneValueParameter(expr) =>
      val constructorType = expr.expressionType(executionResult)
      if(constructorType =!= this) {
        Failure(s"the constructor type should be $name but is ${constructorType.name}")
      } else {
        attempt(expr.evaluateT[BigDecimalOpenlawValue](executionResult))
      }
    case _ =>
      Failure(s"the constructor for $name only handles single values")
  }

  override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[BigDecimalOpenlawValue] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[BigDecimalOpenlawValue](leftValue).get + convert[BigDecimalOpenlawValue](rightValue).get

  override def minus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[BigDecimalOpenlawValue] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[BigDecimalOpenlawValue](leftValue).get - convert[BigDecimalOpenlawValue](rightValue).get

  override def multiply(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[BigDecimalOpenlawValue] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[BigDecimalOpenlawValue](leftValue).get * convert[BigDecimalOpenlawValue](rightValue).get

  override def divide(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[BigDecimalOpenlawValue] = for(
    leftValue <- optLeft;
    rightValue <- optRight if convert[BigDecimalOpenlawValue](rightValue).get =!= BigDecimal(0)) yield convert[BigDecimalOpenlawValue](leftValue).get / convert[BigDecimalOpenlawValue](rightValue).get

  override def internalFormat(value: OpenlawValue): String =
    convert[BigDecimalOpenlawValue](value).get.toString

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
        expr.right.evaluate(executionResult).map(_.get) match {
          case Some(value:BigDecimal) if value === BigDecimal(0) => Some(s"error while evaluating the expression '$expr': division by zero!")
          case _ => None
        }
      case _ =>
        None
    }
  }

  override def getTypeClass: Class[BigDecimal] = classOf[BigDecimal]

  def thisType: VariableType = NumberType
}

trait NumberFormatter {
  def formatNumber(bd:BigDecimal):String = {
    bd.bigDecimal.toPlainString.replaceAll("(\\d)(?=(\\d{3})+$)", "$1,")
  }
}

case object NoTrailingZerosFormatter extends Formatter with NumberFormatter {
  override def format(value: openlaw.OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    attempt(VariableType.convert[BigDecimalOpenlawValue](value).get.bigDecimal.stripTrailingZeros()) map {
      case bd => Seq(FreeText(Text(formatNumber(bd))))
    }
}

case object RawNumberFormatter extends Formatter with NumberFormatter {
  override def format(value: openlaw.OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    attempt(VariableType.convert[BigDecimalOpenlawValue](value).get.bigDecimal.stripTrailingZeros().toPlainString) map {
      case str => Seq(FreeText(Text(str)))
    }
}

case class Rounding(expr:Expression) extends Formatter with NumberFormatter {
  override def format(value: openlaw.OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = {
    expr.evaluate(executionResult)
      .map(VariableType.convert[BigDecimalOpenlawValue](_).get)
      .map(_.toInt).map(rounding => attempt(VariableType.convert[BigDecimalOpenlawValue](value).get.setScale(rounding, RoundingMode.HALF_UP))) match {
        case None => Success(Seq(FreeText(Text(value.toString))))
        case Some(Success(result)) => Success(Seq(FreeText(Text(result.toString))))
        case Some(Failure(e, message)) => Failure(e, message)
      }
  }
}