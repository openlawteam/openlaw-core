package org.adridadou.openlaw.parser.template.variableTypes

import java.time.format.{DateTimeFormatter, TextStyle}
import java.time._
import java.time.temporal.ChronoUnit
import java.util.Locale

import org.adridadou.openlaw.parser.template.{AgreementElement, FormatterDefinition, FreeText, Minus, OneValueParameter, Parameter, Plus, TemplateExecutionResult, Text, ValueOperation}

//import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw.{OpenlawDateTime, OpenlawInt, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.result.{Failure, FailureMessage, Result, Success, attempt}
import org.adridadou.openlaw.result.Implicits.{RichResult, RichResultNel}

abstract class DateTypeTrait(varTypeName:String, converter: (String, Clock) => Result[OpenlawDateTime], formatter:Formatter) extends VariableType(varTypeName) {
  override def defaultFormatter: Formatter = formatter

  override def getTypeClass: Class[OpenlawDateTime] = classOf[OpenlawDateTime]

  def cast(value: String, executionResult:TemplateExecutionResult): Result[OpenlawDateTime] = DateConverter.cast(value, executionResult.clock)

  def internalFormat(value: OpenlawValue): Result[String] = {
    val offset = OffsetDateTime.now().getOffset
    VariableType.convert[OpenlawDateTime](value).map(x => (x.toEpochSecond(offset) * 1000).toString)
  }

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[OpenlawDateTime]] = constructorParams match {
    case OneValueParameter(expr) =>
      expr
        .evaluate(executionResult)
        .flatMap { valueOpt =>
          valueOpt.map(value => VariableType.convert[OpenlawString](value).flatMap(castOrConvert(_, executionResult))).sequence
        }
    case _ =>
      Failure("constructor only handles single value")
  }

  override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult:TemplateExecutionResult): Result[Option[OpenlawDateTime]] =
    combine(optLeft, optRight) {
      case (left, period: Period) => VariableType.convert[OpenlawDateTime](left).map(x => plus(x, period))
      case (left, OpenlawString(str)) =>
        PeriodType
          .cast(str, executionResult)
          .addMessageToFailure(s"you can only make an addition between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${str.getClass.getSimpleName}")
          .toResult
          .flatMap { period => VariableType.convert[OpenlawDateTime](left).map(x => plus(x, period)) }
    }

  override def operationWith(rightType: VariableType, operation: ValueOperation): VariableType = (rightType, operation) match {
    case (_: DateTypeTrait, Minus) => PeriodType
    case _ => this
  }

  def plus(d:OpenlawDateTime, p:Period):OpenlawDateTime = d
    .underlying
    .plusSeconds(p.seconds)
    .plusMinutes(p.minutes)
    .plusHours(p.hours)
    .plusDays(p.days)
    .plusWeeks(p.weeks)
    .plusMonths(p.months)
    .plusYears(p.years)

  override def minus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult:TemplateExecutionResult): Result[Option[OpenlawValue]] =
    combine(optLeft, optRight) {
      case (left, period:Period) => VariableType.convert[OpenlawDateTime](left).map(x => minus(x, period))
      case (left, date:OpenlawDateTime) => VariableType.convert[OpenlawDateTime](left).map(x => minus(x, date))
      case (left, OpenlawString(str)) =>
        PeriodType
          .cast(str, executionResult)
          .addMessageToFailure(s"you can only make an subtraction between a date and a date/period. You are making an addition between a ${left.getClass.getSimpleName} and ${str.getClass.getSimpleName}")
          .toResult
          .flatMap(period => VariableType.convert[OpenlawDateTime](left).map(x => minus(x, period)))
    }

  private def minus(d:OpenlawDateTime, p:Period):OpenlawDateTime = d
    .underlying
    .minusSeconds(p.seconds)
    .minusMinutes(p.minutes)
    .minusHours(p.hours)
    .minusDays(p.days)
    .minusWeeks(p.weeks)
    .minusMonths(p.months)
    .minusYears(p.years)

  private def minus(d1: OpenlawDateTime, d2: OpenlawDateTime): Period = {
    val (from, to) = d2.compareTo(d1) match {
      case 1 => (d1.underlying, d2.underlying)
      case _ => (d2.underlying, d1.underlying)
    }
    var tempDateTime = LocalDateTime.from(from)

    val years = tempDateTime.until(to, ChronoUnit.YEARS)
    tempDateTime = tempDateTime.plusYears(years)

    val weeks = tempDateTime.until(to, ChronoUnit.WEEKS)
    tempDateTime = tempDateTime.plusWeeks(weeks)

    val days = tempDateTime.until(to, ChronoUnit.DAYS)
    tempDateTime = tempDateTime.plusDays(days)

    val hours = tempDateTime.until(to, ChronoUnit.HOURS)
    tempDateTime = tempDateTime.plusHours(hours)

    val minutes = tempDateTime.until(to, ChronoUnit.MINUTES)
    tempDateTime = tempDateTime.plusMinutes(minutes)

    val seconds = tempDateTime.until(to, ChronoUnit.SECONDS)

    Period(
      years = years.toInt,
      weeks = weeks.toInt,
      days = days.toInt,
      hours = hours.toInt,
      minutes = minutes.toInt,
      seconds = seconds.toInt)
  }

  private def castOrConvert(value:String, executionResult:TemplateExecutionResult): Result[OpenlawDateTime] = cast(value, executionResult) recoverWith {
    case _ => converter(value, executionResult.clock)
  }

  override def getFormatter(formatter:FormatterDefinition, executionResult:TemplateExecutionResult): Result[Formatter] = formatter.name.toLowerCase match {
    case "date" => Success(new SimpleDateFormatter)
    case "datetime" => Success(new SimpleDateTimeFormatter)
    case "year" => Success(new YearFormatter)
    case "day_name" => Success(new DayNameFormatter)
    case "day" => Success(new DayFormatter)
    case "month_name" => Success(new MonthNameFormatter)
    case "month" => Success(new MonthFormatter)
    case _ => Failure(s"unknown formatter $formatter for type $varTypeName")
  }

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = operation match {
    case Plus => otherType === PeriodType || otherType === TextType || otherType === LargeTextType
    case Minus => otherType === PeriodType || otherType === TextType || otherType === LargeTextType || otherType === DateType || otherType === DateTimeType
    case _ => false
  }
}

case object DateType extends DateTypeTrait("Date", DateConverter.convertToDate, new SimpleDateFormatter) {
  def thisType: VariableType = DateType
}
case object DateTimeType extends DateTypeTrait("DateTime", DateConverter.convertToDateTime, new SimpleDateTimeFormatter) {
  def thisType: VariableType = DateTimeType
}

object DateConverter {

  def cast(value:String, clock:Clock): Result[OpenlawDateTime] = {
    attempt(LocalDateTime.ofEpochSecond(value.toLong / 1000, 0, ZoneOffset.UTC))
      .map(OpenlawDateTime)
      .recoverWith { case _ => convertToDateTime(value, clock) }
  }

  def convertToDate(d: String, clock:Clock): Result[OpenlawDateTime] = {
    val YMD = """(\d{4})-(\d{1,2})-(\d{1,2})""".r
    val MDY = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
    val DMY = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
    d match {
      case YMD(year, month, day) => toLocalDateTime(year.toInt,month.toInt, day.toInt, clock)
      case MDY(month, day, year) => toLocalDateTime(year.toInt,month.toInt, day.toInt, clock)
      case DMY(day, month, year) => toLocalDateTime(year.toInt,month.toInt, day.toInt, clock)

      case _ =>
        Failure(s"Invalid date format for '$d' !")
    }
  }

  private def toLocalDateTime(year:Int, month:Int, day:Int, clock:Clock): Result[OpenlawDateTime] = Success {
    ZonedDateTime.now(clock)
      .withYear(year)
      .withMonth(month)
      .withDayOfMonth(day)
      .withHour(0)
      .withMinute(0)
      .withSecond(0)
      .withNano(0).toLocalDateTime
  }

  private def toLocalDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, clock:Clock): Result[OpenlawDateTime] =
    toLocalDateTime(year, month, day, clock)
    .map {
      _
        .underlying
        .withHour(hour)
        .withMinute(minute)
        .withSecond(second)
        .withNano(0)
    }

  def convertToDateTime(d: String, clock:Clock): Result[OpenlawDateTime] = {
    val YMD = """(\d{4})-(\d{1,2})-(\d{1,2}) (\d{1,2}):(\d{1,2}):(\d{1,2})""".r
    val MDY = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
    val DMY = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
    d match {
      case YMD(year, month, day,hour,minute,second) => toLocalDateTime(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt, second.toInt, clock)
      case MDY(month, day, year,hour,minute,second) => toLocalDateTime(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt, second.toInt, clock)
      case DMY(day, month, year,hour,minute,second) => toLocalDateTime(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt, second.toInt, clock)
      case _ =>
         Failure(s"Invalid date format! $d")
    }
  }
}

class PatternFormat(pattern: String) extends Formatter {
  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern(pattern, Locale.ENGLISH)

  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    DateHelper.convertToDate(value, executionResult.clock).map { zonedDate =>
      Seq(FreeText(Text(formatter.format(zonedDate.underlying))))
    }
}

class YearFormatter extends PatternFormat("yyyy")
class DayFormatter extends PatternFormat("dd")
class DayNameFormatter extends PatternFormat("EEEE")
class MonthFormatter extends PatternFormat("M")
class MonthNameFormatter extends PatternFormat("MMMM")

class SimpleDateFormatter extends Formatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = DateHelper.convertToDate(value, executionResult.clock).map(zonedDate => {
    val month = zonedDate.underlying.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH)
    Seq(FreeText(Text(s"$month ${zonedDate.underlying.getDayOfMonth}, ${zonedDate.underlying.getYear}")))
  })
}

class SimpleDateTimeFormatter extends Formatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    DateHelper
      .convertToDate(value, executionResult.clock)
      .flatMap { zonedDate =>
        for {
          hourInt <- VariableType.convert[OpenlawInt](zonedDate.getHour)
          minuteInt <- VariableType.convert[OpenlawInt](zonedDate.getMinute)
          secondInt <- VariableType.convert[OpenlawInt](zonedDate.getSecond)
        } yield {
          val hour = String.format("%02d", hourInt.asInstanceOf[Integer])
          val minute = String.format("%02d", minuteInt.asInstanceOf[Integer])
          val second = String.format("%02d", secondInt.asInstanceOf[Integer])
          val month = zonedDate.underlying.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH)
          Seq(FreeText(Text(s"$month ${zonedDate.underlying.getDayOfMonth}, ${zonedDate.underlying.getYear} $hour:$minute:$second")))
        }
      }
}

object DateHelper {
  def prepareDate(date:OpenlawDateTime, clock:Clock): LocalDateTime = {
    val offset = clock.getZone.getRules.getOffset(date.underlying)
    val zonedDate = date.underlying.plusSeconds(offset.getTotalSeconds)
    zonedDate
  }

  def convertToDate(value:OpenlawValue, clock: Clock): Result[LocalDateTime] =
    VariableType.convert[OpenlawDateTime](value).map(date => DateHelper.prepareDate(date, clock))
}