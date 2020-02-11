package org.adridadou.openlaw.parser.template.variableTypes

import VariableType._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw.{OpenlawBigDecimal, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.expressions.{
  Expression,
  ValueExpression
}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode

case object NumberType extends VariableType("Number") {
  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawBigDecimal] = attempt(BigDecimal(value))

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawBigDecimal]] = constructorParams match {
    case OneValueParameter(expr) =>
      expr
        .expressionType(executionResult)
        .flatMap { constructorType =>
          if (constructorType =!= this) {
            Failure(
              s"the constructor type should be $name but is ${constructorType.name}"
            )
          } else {
            expr
              .evaluateT[OpenlawBigDecimal](executionResult)
              .map(_.map(OpenlawBigDecimal))
          }
        }
    case _ =>
      Failure(s"the constructor for $name only handles single values")
  }

  override def plus(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    combine(optLeft, optRight) {
      case (left: OpenlawBigDecimal, right: OpenlawBigDecimal) =>
        Success(left.underlying + right.underlying)
      case (left: OpenlawString, right: OpenlawBigDecimal) =>
        Success(left + right.toString)
      case (left: OpenlawBigDecimal, right: OpenlawString) =>
        Success(left.toString + right)
    }

  override def minus(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawBigDecimal]] =
    combineConverted[OpenlawBigDecimal, OpenlawBigDecimal](optLeft, optRight) {
      case (left, right) => Success(left - right)
    }

  override def multiply(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawBigDecimal]] =
    combineConverted[OpenlawBigDecimal, OpenlawBigDecimal](optLeft, optRight) {
      case (left, right) => Success(left * right)
    }

  override def divide(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawBigDecimal]] =
    combineConverted[OpenlawBigDecimal, OpenlawBigDecimal](optLeft, optRight) {
      case (left, right) if right =!= BigDecimal(0) => Success(left / right)
    }

  override def internalFormat(value: OpenlawValue): Result[String] =
    convert[OpenlawBigDecimal](value).map(_.toString)

  override def getFormatter(
      formatter: FormatterDefinition,
      executionResult: TemplateExecutionResult
  ): Result[Formatter] = formatter.name.toLowerCase match {
    case "notrailingzeros" => Success(NoTrailingZerosFormatter)
    case "raw"             => Success(RawNumberFormatter)
    case "rounding" =>
      formatter.parameters match {
        case Some(param: OneValueParameter) =>
          param.expr
            .expressionType(executionResult)
            .flatMap { exprType =>
              if (exprType === NumberType) Success(Rounding(param.expr))
              else
                Failure(
                  s"Rounding formatting only accepts expressions of type Number. The expression type is ${param.expr
                    .expressionType(executionResult)}"
                )
            }
        case Some(_) =>
          Failure("Rounding formatting only accepts a single parameter")
        case None => Failure("missing parameter for rounding")
      }
    case _ => Success(defaultFormatter)
  }

  override def defaultFormatter: Formatter = NoTrailingZerosFormatter

  override def validateOperation(
      expr: ValueExpression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] = {
    expr.operation match {
      case Divide =>
        expr.right
          .evaluate(executionResult)
          .flatMap {
            case Some(value: OpenlawBigDecimal)
                if value.underlying === BigDecimal(0) =>
              Failure(
                s"error while evaluating the expression '$expr': division by zero!"
              )
            case _ => Success(())
          }
      case _ =>
        Success(())
    }
  }

  override def getTypeClass: Class[OpenlawBigDecimal] =
    classOf[OpenlawBigDecimal]

  def thisType: VariableType = NumberType
}

trait NumberFormatter {
  def formatNumber(bd: BigDecimal): String = {
    val regex = "(\\d)(?=(\\d{3})+$)"
    val fullNumber = bd.bigDecimal.toPlainString
    val split = fullNumber.split("\\.")
    val intNumber = split(0)
    val decimalNumber = split.lift(1)
    val formattedIntNumber = intNumber.replaceAll(regex, "$1,")

    decimalNumber
      .map(d => formattedIntNumber + "." + d)
      .getOrElse(formattedIntNumber)
  }
}

case object NoTrailingZerosFormatter extends Formatter with NumberFormatter {
  override def format(
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    VariableType
      .convert[OpenlawBigDecimal](value)
      .map(_.bigDecimal.stripTrailingZeros())
      .map(bd => List(FreeText(Text(formatNumber(bd)))))
}

case object RawNumberFormatter extends Formatter with NumberFormatter {
  override def format(
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    VariableType
      .convert[OpenlawBigDecimal](value)
      .map(_.bigDecimal.stripTrailingZeros().toPlainString)
      .map(str => List(FreeText(Text(str))))
}

final case class Rounding(expr: Expression)
    extends Formatter
    with NumberFormatter {
  override def format(
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] = {
    expr
      .evaluate(executionResult)
      .flatMap { valueOpt =>
        valueOpt
          .map { evalValue =>
            val x = VariableType
              .convert[OpenlawBigDecimal](evalValue)
              .map(x => x.toInt)
              .flatMap(rounding =>
                VariableType
                  .convert[OpenlawBigDecimal](value)
                  .map(_.setScale(rounding, RoundingMode.HALF_UP))
              )
            x
          } match {
          case None => Success(List(FreeText(Text(value.toString))))
          case Some(Success(result)) =>
            Success(List(FreeText(Text(result.toString))))
          case Some(Failure(e, message)) => Failure(e, message)
        }
      }
  }
}
