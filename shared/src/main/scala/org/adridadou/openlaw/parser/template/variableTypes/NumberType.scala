package org.adridadou.openlaw.parser.template.variableTypes

import scala.util.{Failure, Success, Try}
import VariableType._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template._

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

case object NumberType extends VariableType("Number") {
  override def cast(value: String, executionResult: TemplateExecutionResult): BigDecimal = BigDecimal(value)

  override def construct(constructorParams: Parameter,executionResult: TemplateExecutionResult): Option[Any] = constructorParams match {
    case OneValueParameter(expr) =>
      val constructorType = expr.expressionType(executionResult)
      if(constructorType =!= this) {
        throw new RuntimeException(s"the constructor type should be $name but is ${constructorType.name}")
      }
      expr.evaluate(executionResult)
    case _ => throw new RuntimeException(s"the constructor for $name only handles single values")
  }

  override def plus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[BigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[BigDecimal](leftValue) + convert[BigDecimal](rightValue)

  override def minus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[BigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[BigDecimal](leftValue) - convert[BigDecimal](rightValue)

  override def multiply(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[BigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight) yield convert[BigDecimal](leftValue) * convert[BigDecimal](rightValue)

  override def divide(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[BigDecimal] = for(
    leftValue <- optLeft;
    rightValue <- optRight if convert[BigDecimal](rightValue) =!= BigDecimal(0)) yield convert[BigDecimal](leftValue) / convert[BigDecimal](rightValue)

  override def internalFormat(value: Any): String =
    convert[BigDecimal](value).toString

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
          case Some(value:BigDecimal) if value === BigDecimal(0) => Some(s"error while evaluating the expression '$expr': division by zero!")
          case _ => None
        }
      case _ =>
        None
    }
  }

  def thisType: VariableType = NumberType
}

trait NumberFormatter {
  def formatNumber(bd:BigDecimal):String = {
    bd.bigDecimal.toPlainString.replaceAll("(\\d)(?=(\\d{3})+$)", "$1,")
  }
}

case object NoTrailingZerosFormatter extends Formatter with NumberFormatter {
  override def format(value: Any, executionResult: TemplateExecutionResult):Either[String,Seq[AgreementElement]] = Try(VariableType.convert[BigDecimal](value).bigDecimal.stripTrailingZeros()) match {
    case Success(bd) => Right(Seq(FreeText(Text(formatNumber(bd)))))
    case Failure(ex) => Left(ex.getMessage)
  }
}

case object RawNumberFormatter extends Formatter with NumberFormatter {
  override def format(value: Any, executionResult: TemplateExecutionResult):Either[String,Seq[AgreementElement]] = Try(VariableType.convert[BigDecimal](value).bigDecimal.stripTrailingZeros().toPlainString) match {
    case Success(str) => Right(Seq(FreeText(Text(str))))
    case Failure(ex) => Left(ex.getMessage)
  }
}

case class Rounding(expr:Expression) extends Formatter with NumberFormatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Either[String, Seq[AgreementElement]] = {
    expr.evaluate(executionResult)
      .map(VariableType.convert[BigDecimal])
      .map(_.toInt).map(rounding => Try(VariableType.convert[BigDecimal](value)
        .setScale(rounding, RoundingMode.HALF_UP))) match {
      case None => Right(Seq(FreeText(Text(value.toString))))
      case Some(Success(result)) => Right(Seq(FreeText(Text(result.toString))))
      case Some(Failure(ex)) => Left(ex.getMessage)
    }
  }
}