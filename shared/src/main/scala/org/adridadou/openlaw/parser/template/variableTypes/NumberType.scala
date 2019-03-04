package org.adridadou.openlaw.parser.template.variableTypes

import VariableType._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import org.adridadou.openlaw.result.Implicits.eqFailureCause

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

case object NumberType extends VariableType("Number") {
  override def cast(value: String, executionResult: TemplateExecutionResult): BigDecimal = BigDecimal(value)

  override def construct(constructorParams: Parameter,executionResult: TemplateExecutionResult): Result[Option[BigDecimal]] = constructorParams match {
    case OneValueParameter(expr) =>
      val constructorType = expr.expressionType(executionResult)
      if(constructorType =!= this) {
        Failure(s"the constructor type should be $name but is ${constructorType.name}")
      } else {
        expr.evaluateT[BigDecimal](executionResult).sequence
      }
    case _ =>
      Failure(s"the constructor for $name only handles single values")
  }

  override def plus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Result[BigDecimal]] =
    for {
      leftValue <- optLeft
      rightValue <- optRight
    } yield {
      for {
        leftConverted <- convert[BigDecimal](leftValue)
        rightConverted <- convert[BigDecimal](rightValue)
      } yield leftConverted + rightConverted
    }

  override def minus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Result[BigDecimal]] =
    for {
      leftValue <- optLeft
      rightValue <- optRight
    } yield {
      for {
        leftConverted <- convert[BigDecimal](leftValue)
        rightConverted <- convert[BigDecimal](rightValue)
      } yield leftConverted - rightConverted
    }

  override def multiply(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Result[BigDecimal]] =
    for {
      leftValue <- optLeft
      rightValue <- optRight
    } yield {
      for {
        leftConverted <- convert[BigDecimal](leftValue)
        rightConverted <- convert[BigDecimal](rightValue)
      } yield leftConverted * rightConverted
    }

  override def divide(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Result[BigDecimal]] =
    for {
      leftValue <- optLeft
      rightValue <- optRight if convert[BigDecimal](rightValue) =!= Success(BigDecimal(0))
    } yield {
      for {
        leftConverted <- convert[BigDecimal](leftValue)
        rightConverted <- convert[BigDecimal](rightValue)
      } yield leftConverted / rightConverted
    }

  override def internalFormat(value: Any): Result[String] =
    convert[BigDecimal](value).map(_.toString)

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


  override def validateOperation(expr: ValueExpression, executionResult: TemplateExecutionResult): Result[Unit] = {
    expr.operation match {
      case Divide =>
        expr
          .right
          .evaluate(executionResult)
          .sequence
          .flatMap { option =>
            option match {
              case Some(value: BigDecimal) if value === BigDecimal(0) => Failure(s"error while evaluating the expression '$expr': division by zero!")
              case _ => Success(())
            }
          }
      case _ =>
        Success(())
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
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    attempt(VariableType.convert[BigDecimal](value).map(_.bigDecimal.stripTrailingZeros())).flatten map {
      case bd => Seq(FreeText(Text(formatNumber(bd))))
    }
}

case object RawNumberFormatter extends Formatter with NumberFormatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    attempt(VariableType.convert[BigDecimal](value).map(_.bigDecimal.stripTrailingZeros().toPlainString)).flatten map {
      case str => Seq(FreeText(Text(str)))
    }
}

case class Rounding(expr:Expression) extends Formatter with NumberFormatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = {
    expr.evaluate(executionResult)
      .map(VariableType.convert[BigDecimal])
      .map { roundingResult =>
        roundingResult.flatMap(rounding => attempt(VariableType.convert[BigDecimal](value).map(_.setScale(rounding.toInt, RoundingMode.HALF_UP))).flatten)
      } match {
        case None => Success(Seq(FreeText(Text(value.toString))))
        case Some(Success(result)) => Success(Seq(FreeText(Text(result.toString))))
        case Some(Failure(e, message)) => Failure(e, message)
      }
  }
}
