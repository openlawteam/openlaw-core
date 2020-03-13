package org.adridadou.openlaw.parser.template.variableTypes

import org.parboiled2._
import VariableType._
import cats.implicits._
import org.adridadou.openlaw.parser.template.{
  Divide,
  Minus,
  Plus,
  TemplateExecutionResult,
  ValueOperation
}
import org.adridadou.openlaw.{
  OpenlawBigDecimal,
  OpenlawInstant,
  OpenlawNativeValue,
  OpenlawValue
}
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.parser.template.expressions.{
  Expression,
  ValueExpression
}

import scala.language.implicitConversions
import scala.math.BigDecimal

case object PeriodType extends VariableType("Period") {

  override def plus(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[
    OpenlawValue
  ]] =
    for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
      result <- plus(leftValue, rightValue, executionResult)
    } yield result

  def plus(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    combine(optLeft, optRight) {
      case (left, period: Period) => convert[Period](left).map(plus(_, period))
      case (left, date: OpenlawInstant) =>
        convert[Period](left).map(DateTimeType.plus(date, _))
    }

  private def plus(left: Period, right: Period): Period = left.plus(right)

  override def minus(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[Period]] =
    for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
      result <- minus(leftValue, rightValue, executionResult)
    } yield result

  def minus(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[Period]] =
    combineConverted[Period, Period](optLeft, optRight) {
      case (left, right) => Success(minus(left, right))
    }

  private def minus(left: Period, right: Period): Period = left.minus(right)

  override def divide(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[Period]] =
    for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
      result <- divide(leftValue, rightValue, executionResult)
    } yield result

  def divide(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[Period]] =
    combineConverted[Period, OpenlawBigDecimal, Period](optLeft, optRight) {
      case (left, right) if right =!= BigDecimal(0) =>
        Success(divide(left, right))
    }

  private def divide(left: Period, right: OpenlawBigDecimal): Period =
    left.divide(right)

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[Period] =
    cast(value)

  def cast(value: String): Result[Period] = {
    val parser = new PeriodTypeParser(value)
    parser.root.run().toEither match {
      case Right(res) =>
        Success(res)
      case Left(ex: ParseError) =>
        Failure(parser.formatError(ex))
      case Left(ex: Exception) =>
        Failure(ex)

      case Left(ex) =>
        throw ex // Do not try to handle fatal error
    }
  }

  override def isCompatibleType(
      otherType: VariableType,
      operation: ValueOperation
  ): Boolean = operation match {
    case Plus =>
      otherType === PeriodType || otherType === DateType || otherType === DateTimeType
    case Minus =>
      otherType === PeriodType || otherType === DateType || otherType === DateTimeType
    case Divide => otherType === NumberType
    case _      => false
  }

  override def internalFormat(value: OpenlawValue): Result[String] = {
    convert[Period](value).map { period =>
      val result = (if (period.years > 1) s"${period.years}" + " years "
                    else if (period.years === 1) s"${period.years}" + " year "
                    else "") +
        (if (period.months > 1) s"${period.months}" + " months "
         else if (period.months === 1) s"${period.months}" + " month "
         else "") +
        (if (period.weeks > 1) s"${period.weeks}" + " weeks "
         else if (period.weeks === 1) s"${period.weeks}" + " week "
         else "") +
        (if (period.days > 1) s"${period.days}" + " days "
         else if (period.days === 1) s"${period.days}" + " day "
         else "") +
        (if (period.hours > 1) s"${period.hours}" + " hours "
         else if (period.hours === 1) s"${period.hours}" + " hour "
         else "") +
        (if (period.minutes > 1) s"${period.minutes}" + " minutes "
         else if (period.minutes === 1) s"${period.minutes}" + " minute "
         else "") +
        (if (period.seconds > 1) s"${period.seconds}" + " seconds "
         else if (period.seconds === 1) s"${period.seconds}" + " second "
         else "")
      result
    }
  }

  override def getTypeClass: Class[_ <: Period] = classOf[Period]

  def thisType: VariableType = PeriodType

  override def operationWith(
      rightType: VariableType,
      operation: ValueOperation
  ): VariableType = (rightType, operation) match {
    case (NumberType, Divide) => PeriodType
    case _                    => rightType
  }

  override def validateOperation(
      expr: ValueExpression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] =
    expr.operation match {
      case Divide =>
        (for {
          leftOption <- expr.left.evaluate(executionResult)
          rightOption <- expr.right.evaluate(executionResult)
        } yield {
          (leftOption, rightOption) match {
            case (_, Some(value: OpenlawBigDecimal))
                if value.underlying === BigDecimal(0) =>
              Failure(
                s"error while evaluating the expression '$expr': division by zero!"
              )
            case (Some(period: Period), _) if period.months > 0 =>
              Failure(
                s"error while evaluating the expression '$expr': cannot divide months"
              )
            case _ => Success(None)
          }
        }).flatten.map(_ => ())
      case _ =>
        Success(())
    }
}

class PeriodTypeParser(val input: ParserInput) extends Parser {

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def digits: Rule1[Int] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((str: String) => str.toInt)
  }

  def periodEntry: Rule1[(Int, String)] = rule {
    digits ~ zeroOrMore(' ') ~ capture(oneOrMore(CharPredicate.Alpha)) ~> (
        (
            num: Int,
            str: String
        ) => (num, str)
    )
  }

  def root: Rule1[Period] = rule {
    oneOrMore(zeroOrMore(' ') ~ periodEntry ~ zeroOrMore(' ')) ~ EOI ~> (
        (values: Seq[(Int, String)]) =>
          values
            .map({
              case (digit, periodType) =>
                periodType.trim match {
                  case "year"  => Period(years = digit)
                  case "years" => Period(years = digit)

                  case "month"  => Period(months = digit)
                  case "months" => Period(months = digit)

                  case "week"  => Period(weeks = digit)
                  case "weeks" => Period(weeks = digit)

                  case "day"  => Period(days = digit)
                  case "days" => Period(days = digit)

                  case "hour"  => Period(hours = digit)
                  case "hours" => Period(hours = digit)

                  case "minute"  => Period(minutes = digit)
                  case "minutes" => Period(minutes = digit)

                  case "second"  => Period(seconds = digit)
                  case "seconds" => Period(seconds = digit)

                  case other =>
                    throw new RuntimeException(
                      other + " is not a valid period type"
                    )
                }
            })
            .reduce((left, right) => left.plus(right))
      )
  }
}

final case class ParameterNotFound(value: String) extends RuntimeException
final case class ParsingError(msg: String) extends RuntimeException

final case class Period(
    seconds: Int = 0,
    minutes: Int = 0,
    hours: Int = 0,
    days: Int = 0,
    weeks: Int = 0,
    months: Int = 0,
    years: Int = 0
) extends OpenlawNativeValue {
  def minus(right: Period): Period =
    Period(
      seconds - right.seconds,
      minutes - right.minutes,
      hours - right.hours,
      days - right.days,
      weeks - right.weeks,
      months - right.months,
      years - right.years
    )
  def plus(right: Period): Period =
    Period(
      seconds + right.seconds,
      minutes + right.minutes,
      hours + right.hours,
      days + right.days,
      weeks + right.weeks,
      months + right.months,
      years + right.years
    )
  def divide(right: OpenlawBigDecimal): Period = {
    var totalSeconds = toSeconds / right.underlying.toLongExact

    val divYears = totalSeconds / (365 * 24 * 60 * 60)
    totalSeconds -= divYears * (365 * 24 * 60 * 60)
    val divWeeks = totalSeconds / (7 * 24 * 60 * 60)
    totalSeconds -= divWeeks * (7 * 24 * 60 * 60)
    val divDays = totalSeconds / (24 * 60 * 60)
    totalSeconds -= divDays * (24 * 60 * 60)
    val divHours = totalSeconds / (60 * 60)
    totalSeconds -= divHours * (60 * 60)
    val divMinutes = totalSeconds / 60
    totalSeconds -= divMinutes * 60

    Period(
      years = divYears.toInt,
      weeks = divWeeks.toInt,
      days = divDays.toInt,
      hours = divHours.toInt,
      minutes = divMinutes.toInt,
      seconds = totalSeconds.toInt
    )
  }
  override def toString: String =
    (if (years > 1) s"$years years "
     else if (years === 1) s"$years year "
     else "") +
      (if (months > 1) s"$months months "
       else if (months === 1) s"$months month "
       else "") +
      (if (weeks > 1) s"$weeks weeks "
       else if (weeks === 1) s"$weeks week "
       else "") +
      (if (days > 1) s"$days days " else if (days === 1) s"$days day " else "") +
      (if (hours > 1) s"$hours hours "
       else if (hours === 1) s"$hours hour "
       else "") +
      (if (minutes > 1) s"$minutes minutes "
       else if (minutes === 1) s"$minutes minute "
       else "") +
      (if (seconds > 1) s"$seconds seconds"
       else if (seconds === 1) s"$seconds second"
       else "")

  private def toSeconds: Long = {
    seconds.toLong + 60 * (minutes.toLong + 60 * (hours.toLong + 24 * (days.toLong + 7 * weeks.toLong + 365 * years.toLong)))
  }
}
