package org.adridadou.openlaw.parser.template.variableTypes

import java.time.format.{DateTimeFormatter, TextStyle}
import java.time._
import java.time.temporal.ChronoUnit
import java.util.Locale

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw.{OpenlawDateTime, OpenlawInt, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

abstract class DateTypeTrait(varTypeName:String, converter: (String, Clock) => OpenlawDateTime, formatter:Formatter) extends VariableType(varTypeName) {
  override def defaultFormatter: Formatter = formatter

  override def getTypeClass: Class[OpenlawDateTime] = classOf[OpenlawDateTime]

  def cast(value: String, executionResult:TemplateExecutionResult):OpenlawDateTime = DateConverter.cast(value, executionResult.clock)
  def internalFormat(value: OpenlawValue): String = {
    val offset = OffsetDateTime.now().getOffset
    (VariableType.convert[OpenlawDateTime](value).underlying.toEpochSecond(offset) * 1000).toString
  }

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[OpenlawDateTime]] = constructorParams match {
    case OneValueParameter(expr) =>
      attempt(expr.evaluate(executionResult).map(value => castOrConvert(VariableType.convert[OpenlawString](value), executionResult)))
    case _ =>
      Failure("constructor only handles single value")
  }

  override def operationWith(rightType: VariableType, operation: ValueOperation): VariableType = (rightType, operation) match {
    case (_: DateTypeTrait, Minus) => PeriodType
    case _ => this
  }

  override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult:TemplateExecutionResult): Option[OpenlawDateTime] = for {
    left <- optLeft
    right <- optRight
  } yield {
    right match {
      case period:Period => plus(VariableType.convert[OpenlawDateTime](left), period)
      case OpenlawString(str) =>
        attempt(PeriodType.cast(str, executionResult)) match {
          case Success(period) => plus(VariableType.convert[OpenlawDateTime](left), period)
          case Failure(_,_) => throw new RuntimeException(s"you can only make an addition between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
        }
      case _ => throw new RuntimeException(s"you can only make an addition between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
    }
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

  override def minus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult:TemplateExecutionResult): Option[OpenlawValue] = for {
    left <- optLeft
    right <- optRight
  } yield {
    right match {
      case period:Period => minus(VariableType.convert[OpenlawDateTime](left), period)
      case date:OpenlawDateTime => minus(VariableType.convert[OpenlawDateTime](left), date)
      case OpenlawString(str) =>
        attempt(PeriodType.cast(str, executionResult)) match {
          case Success(period) => minus(VariableType.convert[OpenlawDateTime](left), period)
          case Failure(_,_) => throw new RuntimeException(s"you can only make a substraction between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
        }
      case _ => throw new RuntimeException(s"you can only make a substraction between a date and a date/period. You are making an substraction between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
    }
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

    val months = tempDateTime.until(to, ChronoUnit.MONTHS)
    tempDateTime = tempDateTime.plusMonths(months)

    val days = tempDateTime.until(to, ChronoUnit.DAYS)
    tempDateTime = tempDateTime.plusDays(days)

    val hours = tempDateTime.until(to, ChronoUnit.HOURS)
    tempDateTime = tempDateTime.plusHours(hours)

    val minutes = tempDateTime.until(to, ChronoUnit.MINUTES)
    tempDateTime = tempDateTime.plusMinutes(minutes)

    val seconds = tempDateTime.until(to, ChronoUnit.SECONDS)

    Period(
      years = years.toInt,
      months = months.toInt,
      days = days.toInt,
      hours = hours.toInt,
      minutes = minutes.toInt,
      seconds = seconds.toInt)
  }

  private def castOrConvert(value:String, executionResult:TemplateExecutionResult):OpenlawDateTime = attempt(cast(value, executionResult)) match {
    case Success(date) => date
    case _ => converter(value, executionResult.clock)
  }

  override def getFormatter(formatter:FormatterDefinition, executionResult:TemplateExecutionResult): Formatter = formatter.name.toLowerCase match {
    case "date" => new SimpleDateFormatter
    case "datetime" => new SimpleDateTimeFormatter
    case "year" => new YearFormatter
    case "day_name" => new DayNameFormatter
    case "day" => new DayFormatter
    case "month_name" => new MonthNameFormatter
    case "month" => new MonthFormatter
    case _ => throw new RuntimeException(s"unknown formatter $formatter for type $varTypeName")
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

  def cast(value:String, clock:Clock): OpenlawDateTime = attempt(LocalDateTime.ofEpochSecond(value.toLong / 1000, 0, ZoneOffset.UTC)) match {
    case Success(x) => x
    case _ => convertToDateTime(value, clock)
  }

  def convertToDate(d: String, clock:Clock): OpenlawDateTime = {
    val YMD = """(\d{4})-(\d{1,2})-(\d{1,2})""".r
    val MDY = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
    val DMY = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
    d match {
      case YMD(year, month, day) => toLocalDateTime(year.toInt,month.toInt, day.toInt, clock)
      case MDY(month, day, year) => toLocalDateTime(year.toInt,month.toInt, day.toInt, clock)
      case DMY(day, month, year) => toLocalDateTime(year.toInt,month.toInt, day.toInt, clock)

      case _ =>
        throw new RuntimeException(s"Invalid date format for '$d' !")
    }
  }

  private def toLocalDateTime(year:Int, month:Int, day:Int, clock:Clock):OpenlawDateTime = ZonedDateTime.now(clock)
    .withYear(year)
    .withMonth(month)
    .withDayOfMonth(day)
    .withHour(0)
    .withMinute(0)
    .withSecond(0)
    .withNano(0).toLocalDateTime

  private def toLocalDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, clock:Clock):OpenlawDateTime =
    toLocalDateTime(year,month,day, clock)
      .underlying
      .withHour(hour)
      .withMinute(minute)
      .withSecond(second)
      .withNano(0)


  def convertToDateTime(d: String, clock:Clock): OpenlawDateTime = {
    val YMD = """(\d{4})-(\d{1,2})-(\d{1,2}) (\d{1,2}):(\d{1,2}):(\d{1,2})""".r
    val MDY = """(\d{1,2})/(\d{1,2})/(\d{4})""".r
    val DMY = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
    d match {
      case YMD(year, month, day,hour,minute,second) => toLocalDateTime(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt, second.toInt, clock)
      case MDY(month, day, year,hour,minute,second) => toLocalDateTime(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt, second.toInt, clock)
      case DMY(day, month, year,hour,minute,second) => toLocalDateTime(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt, second.toInt, clock)
      case _ =>
         throw new RuntimeException(s"Invalid date format! $d")
    }
  }
}

class PatternFormat(pattern: String) extends Formatter {
  val formatter = DateTimeFormatter.ofPattern(pattern, Locale.ENGLISH)

  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = DateHelper.convertToDate(value, executionResult.clock).map(zonedDate => {
    Seq(FreeText(Text(formatter.format(zonedDate.underlying))))
  })
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

class SimpleDateTimeFormatter extends Formatter{
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = DateHelper.convertToDate(value, executionResult.clock).map(zonedDate => {
    val hour = String.format("%02d", Integer.valueOf(VariableType.convert[OpenlawInt](zonedDate.getHour)))
    val minute = String.format("%02d", Integer.valueOf(VariableType.convert[OpenlawInt](zonedDate.getMinute)))
    val second = String.format("%02d", Integer.valueOf(VariableType.convert[OpenlawInt](zonedDate.getSecond)))
    val month = zonedDate.underlying.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH)
    Seq(FreeText(Text(s"$month ${zonedDate.underlying.getDayOfMonth}, ${zonedDate.underlying.getYear} $hour:$minute:$second")))
  })
}

object DateHelper {
  def prepareDate(date:OpenlawDateTime, clock:Clock): LocalDateTime = {
    val offset = clock.getZone.getRules.getOffset(date.underlying)
    val zonedDate = date.underlying.plusSeconds(offset.getTotalSeconds)
    zonedDate
  }

  def convertToDate(value:OpenlawValue, clock: Clock): Result[LocalDateTime] = attempt(VariableType.convert[OpenlawDateTime](value)) map {
    case date => DateHelper.prepareDate(date, clock)
  }
}