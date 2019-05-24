package org.adridadou.openlaw.parser.template.variableTypes

import java.time.format.{DateTimeFormatter, TextStyle}
import java.time._
import java.util.Locale

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.Formatter
import cats.implicits._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

abstract class DateTypeTrait(varTypeName:String, converter: (String, Clock) => LocalDateTime, formatter:Formatter) extends VariableType(varTypeName) {
  override def defaultFormatter: Formatter = formatter

  override def getTypeClass: Class[LocalDateTime] = classOf[LocalDateTime]

  def cast(value: String, executionResult:TemplateExecutionResult):LocalDateTime  = DateConverter.cast(value, executionResult.clock)
  def internalFormat(value: Any): String = {
    val offset = OffsetDateTime.now().getOffset
    (VariableType.convert[LocalDateTime](value).toEpochSecond(offset) * 1000).toString
  }

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[LocalDateTime]] = constructorParams match {
    case OneValueParameter(expr) =>
      attempt(expr.evaluate(executionResult).map(value => castOrConvert(VariableType.convert[String](value), executionResult)))
    case _ =>
      Failure("constructor only handles single value")
  }

  override def plus(optLeft: Option[Any], optRight: Option[Any], executionResult:TemplateExecutionResult): Option[LocalDateTime] = for {
    left <- optLeft
    right <- optRight
  } yield {
    right match {
      case period:Period => plus(VariableType.convert[LocalDateTime](left), period)
      case str:String =>
        attempt(PeriodType.cast(str, executionResult)) match {
          case Success(period) => plus(VariableType.convert[LocalDateTime](left), period)
          case Failure(_,_) => throw new RuntimeException(s"you can only make an addition between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
        }
      case _ => throw new RuntimeException(s"you can only make an addition between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
    }
  }

  def plus(d:LocalDateTime, p:Period):LocalDateTime = d
    .plusSeconds(p.seconds)
    .plusMinutes(p.minutes)
    .plusHours(p.hours)
    .plusDays(p.days)
    .plusWeeks(p.weeks)
    .plusMonths(p.months)
    .plusYears(p.years)

  override def minus(optLeft: Option[Any], optRight: Option[Any], executionResult:TemplateExecutionResult): Option[LocalDateTime] = for {
    left <- optLeft
    right <- optRight
  } yield {
    right match {
      case period:Period => minus(VariableType.convert[LocalDateTime](left), period)
      case str:String =>
        attempt(PeriodType.cast(str, executionResult)) match {
          case Success(period) => minus(VariableType.convert[LocalDateTime](left), period)
          case Failure(_,_) => throw new RuntimeException(s"you can only make a substraction between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
        }
      case _ => throw new RuntimeException(s"you can only make a substraction between a date and a period. You are making an addition between a ${left.getClass.getSimpleName} and ${right.getClass.getSimpleName}")
    }
  }

  private def minus(d:LocalDateTime, p:Period):LocalDateTime = d
    .minusSeconds(p.seconds)
    .minusMinutes(p.minutes)
    .minusHours(p.hours)
    .minusDays(p.days)
    .minusWeeks(p.weeks)
    .minusMonths(p.months)
    .minusYears(p.years)

  private def castOrConvert(value:String, executionResult:TemplateExecutionResult):LocalDateTime = attempt(cast(value, executionResult)) match {
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
    case Minus => otherType === PeriodType || otherType === TextType || otherType === LargeTextType
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

  def cast(value:String, clock:Clock): LocalDateTime = attempt(LocalDateTime.ofEpochSecond(value.toLong / 1000, 0, ZoneOffset.UTC)) match {
    case Success(x) => x
    case _ => convertToDateTime(value, clock)
  }

  def convertToDate(d: String, clock:Clock): LocalDateTime = {
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

  private def toLocalDateTime(year:Int, month:Int, day:Int, clock:Clock):LocalDateTime = ZonedDateTime.now(clock)
    .withYear(year)
    .withMonth(month)
    .withDayOfMonth(day)
    .withHour(0)
    .withMinute(0)
    .withSecond(0)
    .withNano(0).toLocalDateTime

  private def toLocalDateTime(year:Int, month:Int, day:Int, hour:Int, minute:Int, second:Int, clock:Clock):LocalDateTime =
    toLocalDateTime(year,month,day, clock)
      .withHour(hour)
      .withMinute(minute)
      .withSecond(second)
      .withNano(0)


  def convertToDateTime(d: String, clock:Clock): LocalDateTime = {
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

  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = DateHelper.convertToDate(value, executionResult.clock).map(zonedDate => {
    Seq(FreeText(Text(formatter.format(zonedDate))))
  })
}

class YearFormatter extends PatternFormat("yyyy")
class DayFormatter extends PatternFormat("dd")
class DayNameFormatter extends PatternFormat("EEEE")
class MonthFormatter extends PatternFormat("M")
class MonthNameFormatter extends PatternFormat("MMMM")

class SimpleDateFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = DateHelper.convertToDate(value, executionResult.clock).map(zonedDate => {
    val month = zonedDate.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH)
    Seq(FreeText(Text(s"$month ${zonedDate.getDayOfMonth}, ${zonedDate.getYear}")))
  })
}

class SimpleDateTimeFormatter extends Formatter{
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = DateHelper.convertToDate(value, executionResult.clock).map(zonedDate => {
    val hour = String.format("%02d", VariableType.convert[java.lang.Integer](zonedDate.getHour))
    val minute = String.format("%02d", VariableType.convert[java.lang.Integer](zonedDate.getMinute))
    val second = String.format("%02d", VariableType.convert[java.lang.Integer](zonedDate.getSecond))
    val month = zonedDate.getMonth.getDisplayName(TextStyle.FULL, Locale.ENGLISH)
    Seq(FreeText(Text(s"$month ${zonedDate.getDayOfMonth}, ${zonedDate.getYear} $hour:$minute:$second")))
  })
}

object DateHelper {
  def prepareDate(date:LocalDateTime, clock:Clock): LocalDateTime = {
    val offset = clock.getZone.getRules.getOffset(date)
    val zonedDate = date.plusSeconds(offset.getTotalSeconds)
    zonedDate
  }

  def convertToDate(value:Any, clock: Clock): Result[LocalDateTime] = attempt(VariableType.convert[LocalDateTime](value)) map {
    case date => DateHelper.prepareDate(date, clock)
  }
}