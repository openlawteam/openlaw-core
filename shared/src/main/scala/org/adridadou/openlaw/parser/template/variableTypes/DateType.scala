package org.adridadou.openlaw.parser.template.variableTypes

import java.time.format.{DateTimeFormatter, TextStyle}
import java.time._
import java.time.temporal.ChronoUnit
import java.util.Locale

import org.adridadou.openlaw.parser.template.{
  AgreementElement,
  FormatterDefinition,
  FreeText,
  Minus,
  OneValueParameter,
  Parameter,
  Plus,
  TemplateExecutionResult,
  Text,
  ValueOperation
}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.{
  OpenlawInstant,
  OpenlawInt,
  OpenlawString,
  OpenlawValue
}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import org.adridadou.openlaw.result.Implicits.{RichResult, RichResultNel}

abstract class DateTypeTrait(
    varTypeName: String,
    converter: (String, TemplateExecutionResult) => Result[OpenlawInstant],
    formatter: Formatter
) extends VariableType(varTypeName) {
  override def defaultFormatter: Formatter = formatter

  override def getTypeClass: Class[OpenlawInstant] = classOf[OpenlawInstant]

  def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawInstant] = DateConverter.cast(value, executionResult)

  def internalFormat(value: OpenlawValue): Result[String] =
    VariableType
      .convert[OpenlawInstant](value)
      .map(x => x.underlying.toEpochMilli.toString)

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawInstant]] = constructorParams match {
    case OneValueParameter(expr) =>
      expr
        .evaluate(executionResult)
        .flatMap { valueOpt =>
          valueOpt
            .map(value =>
              VariableType
                .convert[OpenlawString](value)
                .flatMap(castOrConvert(_, executionResult))
            )
            .sequence
        }
    case _ =>
      Failure("constructor only handles single value")
  }

  override def plus(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    for {
      leftValue <- left.evaluate(executionResult)
      leftType <- left.expressionType(executionResult)
      rightValue <- right.evaluate(executionResult)
      result <- plus(left, leftValue, leftType, rightValue, executionResult)
    } yield result

  def plus(
      left: Expression,
      leftValue: Option[OpenlawValue],
      leftType: VariableType,
      rightValue: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] = {
    (leftValue, rightValue) match {
      case (Some(l: OpenlawInstant), Some(period: Period)) =>
        Success(Some(plus(l, period)))
      case (Some(l: OpenlawString), Some(period: Period)) =>
        attempt(Instant.ofEpochMilli(l.underlying.toLong)).map(i =>
          Some(plus(i, period))
        )
      case (Some(l), Some(OpenlawString(str))) =>
        PeriodType.cast(str, executionResult) match {
          case Success(period) =>
            VariableType
              .convert[OpenlawInstant](l)
              .map(x => Some(plus(x, period)))
          case Failure(ex, message) =>
            leftType.defaultFormatter
              .stringFormat(left, l, executionResult)
              .map(s => Some(OpenlawString(s)))

        }
      case (Some(l), Some(r)) => Failure("not good!!")
      case _                  => Success(None)
    }
  }

  override def operationWith(
      rightType: VariableType,
      operation: ValueOperation
  ): VariableType = (rightType, operation) match {
    case (_: DateTypeTrait, Minus) => PeriodType
    case (TextType, Plus)          => TextType
    case _                         => this
  }

  def plus(d: OpenlawInstant, p: Period): OpenlawInstant = {
    val zonedDate = ZonedDateTime
      .ofInstant(d.underlying, Clock.systemDefaultZone().getZone)
    zonedDate
      .plusSeconds(p.seconds)
      .plusMinutes(p.minutes)
      .plusHours(p.hours)
      .plusDays(p.days)
      .plusWeeks(p.weeks)
      .plusMonths(p.months)
      .plusYears(p.years)
      .toInstant
  }

  override def minus(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
      result <- minus(leftValue, rightValue, executionResult)
    } yield result

  def minus(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    combine(optLeft, optRight) {
      case (left, period: Period) =>
        VariableType.convert[OpenlawInstant](left).map(x => minus(x, period))
      case (left, date: OpenlawInstant) =>
        VariableType.convert[OpenlawInstant](left).map(x => minus(x, date))
      case (left, OpenlawString(str)) =>
        PeriodType
          .cast(str, executionResult)
          .addMessageToFailure(
            s"you can only make an subtraction between a date and a date/period. You are making an addition between a ${left.getClass.getSimpleName} and ${str.getClass.getSimpleName}"
          )
          .toResult
          .flatMap(period =>
            VariableType
              .convert[OpenlawInstant](left)
              .map(x => minus(x, period))
          )
    }

  private def minus(d: OpenlawInstant, p: Period): OpenlawInstant =
    d.underlying
      .minusSeconds(p.seconds)
      .minus(p.minutes, ChronoUnit.MINUTES)
      .minus(p.hours, ChronoUnit.HOURS)
      .minus(p.days, ChronoUnit.DAYS)
      .minus(p.weeks, ChronoUnit.WEEKS)
      .minus(p.months, ChronoUnit.MONTHS)
      .minus(p.years, ChronoUnit.YEARS)

  private def minus(d1: OpenlawInstant, d2: OpenlawInstant): Period = {
    val (from, to) = d2.compareTo(d1) match {
      case 1 => (d1.underlying, d2.underlying)
      case _ => (d2.underlying, d1.underlying)
    }
    var tempDateTime =
      ZonedDateTime.ofInstant(from, Clock.systemDefaultZone().getZone)
    val toDateTime =
      ZonedDateTime.ofInstant(to, Clock.systemDefaultZone().getZone)

    val years = tempDateTime.until(toDateTime, ChronoUnit.YEARS)
    tempDateTime = tempDateTime.plus(years, ChronoUnit.YEARS)

    val weeks = tempDateTime.until(toDateTime, ChronoUnit.WEEKS)
    tempDateTime = tempDateTime.plus(weeks, ChronoUnit.WEEKS)

    val days = tempDateTime.until(toDateTime, ChronoUnit.DAYS)
    tempDateTime = tempDateTime.plus(days, ChronoUnit.DAYS)

    val hours = tempDateTime.until(toDateTime, ChronoUnit.HOURS)
    tempDateTime = tempDateTime.plus(hours, ChronoUnit.HOURS)

    val minutes = tempDateTime.until(toDateTime, ChronoUnit.MINUTES)
    tempDateTime = tempDateTime.plus(minutes, ChronoUnit.MINUTES)

    val seconds = tempDateTime.until(toDateTime, ChronoUnit.SECONDS)

    Period(
      years = years.toInt,
      weeks = weeks.toInt,
      days = days.toInt,
      hours = hours.toInt,
      minutes = minutes.toInt,
      seconds = seconds.toInt
    )
  }

  private def castOrConvert(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawInstant] = cast(value, executionResult) recoverWith {
    case _ => converter(value, executionResult)
  }

  override def getFormatter(
      formatter: FormatterDefinition,
      executionResult: TemplateExecutionResult
  ): Result[Formatter] = formatter.name.toLowerCase match {
    case "date"       => Success(SimpleDateFormatter)
    case "datetime"   => Success(SimpleDateTimeFormatter)
    case "year"       => Success(YearFormatter)
    case "day_name"   => Success(DayNameFormatter)
    case "day"        => Success(DayFormatter)
    case "month_name" => Success(MonthNameFormatter)
    case "month"      => Success(MonthFormatter)
    case _            => Failure(s"unknown formatter $formatter for type $varTypeName")
  }

  override def isCompatibleType(
      otherType: VariableType,
      operation: ValueOperation
  ): Boolean = operation match {
    case Plus =>
      otherType === PeriodType || otherType === TextType || otherType === LargeTextType
    case Minus =>
      otherType === PeriodType || otherType === TextType || otherType === LargeTextType || otherType === DateType || otherType === DateTimeType
    case _ => false
  }
}

case object DateType
    extends DateTypeTrait(
      "Date",
      DateConverter.convertToDate,
      SimpleDateFormatter
    ) {
  def thisType: VariableType = DateType
}
case object DateTimeType
    extends DateTypeTrait(
      "DateTime",
      DateConverter.convertToDateTime,
      SimpleDateTimeFormatter
    ) {
  def thisType: VariableType = DateTimeType
}

object DateConverter {

  def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawInstant] =
    attempt(Instant.ofEpochMilli(value.toLong))
      .map(OpenlawInstant)
      .recoverWith { case _ => convertToDateTime(value, executionResult) }

  def convertToDate(
      d: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawInstant] = {
    val YMD = """(\d{4})-(\d{1,2})-(\d{1,2})""".r
    val MDY = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
    val DMY = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
    d match {
      case YMD(year, month, day) =>
        toLocalDateTime(year.toInt, month.toInt, day.toInt, executionResult)
      case MDY(month, day, year) =>
        toLocalDateTime(year.toInt, month.toInt, day.toInt, executionResult)
      case DMY(day, month, year) =>
        toLocalDateTime(year.toInt, month.toInt, day.toInt, executionResult)

      case _ =>
        Failure(s"Invalid date format for '$d' !")
    }
  }

  private def toLocalDateTime(
      year: Int,
      month: Int,
      day: Int,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawInstant] = attempt {
    val now = ZonedDateTime
      .ofInstant(executionResult.info.now, Clock.systemDefaultZone().getZone)

    OpenlawInstant(
      now
        .withYear(year)
        .withMonth(month)
        .withDayOfMonth(day)
        .withHour(0)
        .withMinute(0)
        .withSecond(0)
        .withNano(0)
        .toInstant
    )
  }

  private def toLocalDateTime(
      year: Int,
      month: Int,
      day: Int,
      hour: Int,
      minute: Int,
      second: Int,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawInstant] = attempt {
    val now = ZonedDateTime
      .ofInstant(executionResult.info.now, Clock.systemDefaultZone().getZone)

    OpenlawInstant(
      now
        .withYear(year)
        .withMonth(month)
        .withDayOfMonth(day)
        .withHour(hour)
        .withMinute(minute)
        .withSecond(second)
        .withNano(0)
        .toInstant
    )
  }

  def convertToDateTime(
      d: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawInstant] = {
    val YMD = """(\d{4})-(\d{1,2})-(\d{1,2}) (\d{1,2}):(\d{1,2}):(\d{1,2})""".r
    val MDY = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
    val DMY = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
    d match {
      case YMD(year, month, day, hour, minute, second) =>
        toLocalDateTime(
          year.toInt,
          month.toInt,
          day.toInt,
          hour.toInt,
          minute.toInt,
          second.toInt,
          executionResult
        )
      case MDY(month, day, year, hour, minute, second) =>
        toLocalDateTime(
          year.toInt,
          month.toInt,
          day.toInt,
          hour.toInt,
          minute.toInt,
          second.toInt,
          executionResult
        )
      case DMY(day, month, year, hour, minute, second) =>
        toLocalDateTime(
          year.toInt,
          month.toInt,
          day.toInt,
          hour.toInt,
          minute.toInt,
          second.toInt,
          executionResult
        )
      case _ =>
        Failure(s"Invalid date format! $d")
    }
  }
}

class PatternFormat(pattern: String) extends Formatter {
  val formatter: DateTimeFormatter =
    DateTimeFormatter
      .ofPattern(pattern, Locale.ENGLISH)
      .withZone(ZoneId.systemDefault())

  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    stringFormat(expression, value, executionResult).map(s =>
      List(FreeText(Text(s)))
    )

  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(s"[[$expression]]")))

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    for {
      instantValue <- VariableType.convert[OpenlawInstant](value)
      formattedValue <- attempt(formatter.format(instantValue.underlying))
    } yield formattedValue
}

object YearFormatter extends PatternFormat("yyyy")
object DayFormatter extends PatternFormat("dd")
object DayNameFormatter extends PatternFormat("EEEE")
object MonthFormatter extends PatternFormat("M")
object MonthNameFormatter extends PatternFormat("MMMM")

object SimpleDateFormatter extends Formatter {
  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    stringFormat(expression, value, executionResult).map(s =>
      List(FreeText(Text(s)))
    )

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    for {
      instantValue <- VariableType.convert[OpenlawInstant](value)
      zonedDate <- attempt(
        ZonedDateTime
          .ofInstant(instantValue.underlying, Clock.systemDefaultZone().getZone)
      )
    } yield {
      val month = zonedDate.getMonth
        .getDisplayName(TextStyle.FULL, Locale.ENGLISH)

      s"$month ${zonedDate.getDayOfMonth}, ${zonedDate.getYear}"
    }
  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(s"[[$expression]]")))
}

object SimpleDateTimeFormatter extends Formatter {

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    for {
      instantValue <- VariableType.convert[OpenlawInstant](value)
      zonedDate <- attempt(
        ZonedDateTime
          .ofInstant(instantValue.underlying, Clock.systemDefaultZone().getZone)
      )
      hourInt <- VariableType.convert[OpenlawInt](zonedDate.getHour)
      minuteInt <- VariableType.convert[OpenlawInt](zonedDate.getMinute)
      secondInt <- VariableType.convert[OpenlawInt](zonedDate.getSecond)
    } yield {
      val hour = formatNumber(hourInt)
      val minute = formatNumber(minuteInt)
      val second = formatNumber(secondInt)
      val month =
        zonedDate.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH)

      s"$month ${zonedDate.getDayOfMonth}, ${zonedDate.getYear} $hour:$minute:$second"
    }

  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    stringFormat(expression, value, executionResult).map(s =>
      List(FreeText(Text(s)))
    )

  private def formatNumber(value: Integer): String =
    String.format("%02d", value)

  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(s"[[$expression]]")))
}
