package org.adridadou.openlaw.parser.template.variableTypes

import java.time.LocalDateTime

import org.parboiled2._
import VariableType._
import cats.implicits._
import org.adridadou.openlaw.parser.template._

case object PeriodType extends VariableType("Period") {

  override def plus(optLeft: Option[Any], optRight: Option[Any], executionResult:TemplateExecutionResult): Option[Any] = for {
    left <- optLeft
    right <-optRight
  } yield {
    right match {
      case period:Period => plus(convert[Period](left), period)
      case date:LocalDateTime => DateTimeType.plus(date, convert[Period](left))
    }
  }

  private def plus(left:Period, right:Period):Period = left.plus(right)

  override def minus(optLeft: Option[Any], optRight: Option[Any], executionResult:TemplateExecutionResult): Option[Period] = for(
    left <- optLeft;
    right <-optRight
  ) yield minus(convert[Period](left), convert[Period](right))

  private def minus(left:Period, right:Period):Period = left.minus(right)

  override def cast(value: String, executionResult:TemplateExecutionResult): Period = {
    val parser = new PeriodTypeParser(value)
    parser.root.run().toEither match {
      case Right(res) => res
      case Left(ex:ParseError) =>
        throw new RuntimeException(parser.formatError(ex))
      case Left(ex) =>
        throw ex
    }
  }

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = operation match {
    case Plus => otherType === PeriodType || otherType === DateType || otherType === DateTimeType
    case Minus => otherType === PeriodType || otherType === DateType || otherType === DateTimeType
    case _ => false
  }

  override def internalFormat(value: Any): String = {
    val period = convert[Period](value)
    val builder = StringBuilder.newBuilder
    if(period.years > 0) {
      builder.append(s"${period.years} years ")
    }
    if(period.months > 0) {
      builder.append(s"${period.months} months ")
    }
    if(period.weeks > 0) {
      builder.append(s"${period.weeks} weeks ")
    }
    if(period.days > 0) {
      builder.append(s"${period.days} days ")
    }
    if(period.hours > 0) {
      builder.append(s"${period.hours} hours ")
    }
    if(period.minutes > 0) {
      builder.append(s"${period.minutes} minutes ")
    }
    builder.append(s"${period.seconds} seconds")

    builder.toString()
  }

  def thisType: VariableType = PeriodType

  override def operationWith(rightType: VariableType, operation: ValueOperation): VariableType = rightType
}

class PeriodTypeParser(val input: ParserInput) extends Parser {

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ zeroOrMore(' ')
  }

  def digits:Rule1[Int] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((str:String) => str.toInt)
  }

  def periodEntry:Rule1[(Int, String)] = rule {
    digits ~ zeroOrMore(' ') ~ capture(oneOrMore(CharPredicate.Alpha)) ~> ((num:Int, str:String) => (num, str))
  }
  def root:Rule1[Period] = rule {
    oneOrMore(zeroOrMore(' ') ~ periodEntry ~ zeroOrMore(' ')) ~ EOI ~> ((values:Seq[(Int, String)]) => values.map({
        case (digit, periodType) => periodType.trim match {
          case "year" => Period(years = digit)
          case "years" => Period(years = digit)

          case "month" => Period(months = digit)
          case "months" => Period(months = digit)

          case "week" => Period(weeks = digit)
          case "weeks" => Period(weeks = digit)

          case "day" => Period(days = digit)
          case "days" => Period(days = digit)

          case "hour" => Period(hours = digit)
          case "hours" => Period(hours = digit)

          case "minute" => Period(minutes = digit)
          case "minutes" => Period(minutes = digit)

          case "second" => Period(seconds = digit)
          case "seconds" => Period(seconds = digit)

          case other:Any => throw new RuntimeException(other + " is not a valid period type")
        }
      }).reduce((left, right) => left.plus(right)))
  }

  def singularPlural(name:String):Rule0 = rule {
    name | (name + "s")
  }
}

case class ParameterNotFound(value:String) extends RuntimeException
case class ParsingError(msg:String) extends RuntimeException

case class Period(seconds:Int = 0, minutes:Int = 0, hours:Int = 0, days:Int = 0, weeks:Int = 0, months:Int = 0, years:Int = 0) {
  def minus(right:Period):Period = Period(seconds - right.seconds, minutes - right.minutes, hours - right.hours, days - right.days, weeks - right.weeks, months - right.months, years - right.years)
  def plus(right:Period):Period = Period(seconds + right.seconds, minutes + right.minutes, hours + right.hours, days + right.days, weeks + right.weeks, months + right.months, years + right.years)
}
