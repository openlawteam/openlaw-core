package org.adridadou.openlaw.parser.template.variableTypes

import org.parboiled2._
import VariableType._
import cats._
import cats.data.EitherT
import cats.implicits._
import org.adridadou.openlaw.{OpenlawDateTime, OpenlawNativeValue, OpenlawValue}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success}

import scala.language.implicitConversions

case object PeriodType extends VariableType("Period") {

  override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult:TemplateExecutionResult): Result[Option[OpenlawValue]] =
    combine(optLeft, optRight) {
      case (left, period:Period) => convert[Period](left).map(plus(_, period))
      case (left, date: OpenlawDateTime) => convert[Period](left).map(DateTimeType.plus(date, _))
    }

  private def plus(left:Period, right:Period): Period = left.plus(right)

  override def minus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult:TemplateExecutionResult): Result[Option[Period]] =
    combineConverted[Period, Period](optLeft, optRight) {
      case (left, right) => Success(minus(left, right))
    }

  private def minus(left:Period, right:Period):Period = left.minus(right)

  override def cast(value: String, executionResult:TemplateExecutionResult): Result[Period] =
    cast(value)

  def cast(value: String): Result[Period] = {
    val parser = new PeriodTypeParser(value)
    parser.root.run().toEither match {
      case Right(res) =>
        Success(res)
      case Left(ex:ParseError) =>
        Failure(parser.formatError(ex))
      case Left(ex: Exception) =>
        Failure(ex)

      case Left(ex) =>
        throw ex // Do not try to handle fatal error
    }
  }

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = operation match {
    case Plus => otherType === PeriodType || otherType === DateType || otherType === DateTimeType
    case Minus => otherType === PeriodType || otherType === DateType || otherType === DateTimeType
    case _ => false
  }

  override def internalFormat(value: OpenlawValue) =
    convert[Period](value).map { period =>
      val result = (if (period.years > 0) s"${period.years}" + " years " else "") +
        (if (period.months > 0) s"${period.months}" + " months " else "") +
        (if (period.weeks > 0) s"${period.weeks}" + " weeks " else "") +
        (if (period.days > 0) s"${period.days}" + " days " else "") +
        (if (period.minutes > 0) s"${period.minutes}" + " minutes " else "") +
        (if (period.seconds > 0) s"${period.seconds}" + " seconds " else "")
      result
    }

  override def getTypeClass: Class[_ <: Period] = classOf[Period]

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

          case other => throw new RuntimeException(other + " is not a valid period type")
        }
      }).reduce((left, right) => left.plus(right)))
  }

  def singularPlural(name:String):Rule0 = rule {
    name | (name + "s")
  }
}

case class ParameterNotFound(value:String) extends RuntimeException
case class ParsingError(msg:String) extends RuntimeException

case class Period(seconds:Int = 0, minutes:Int = 0, hours:Int = 0, days:Int = 0, weeks:Int = 0, months:Int = 0, years:Int = 0) extends OpenlawNativeValue {
  def minus(right:Period):Period = Period(seconds - right.seconds, minutes - right.minutes, hours - right.hours, days - right.days, weeks - right.weeks, months - right.months, years - right.years)
  def plus(right:Period):Period = Period(seconds + right.seconds, minutes + right.minutes, hours + right.hours, days + right.days, weeks + right.weeks, months + right.months, years + right.years)
  override def toString:String = (if( years > 0 ) s"$years years " else "") +
      ( if( months > 0 ) s"$months months " else "") +
      ( if( weeks > 0 ) s"$weeks weeks " else "") +
      ( if( days > 0 ) s"$days days " else "") +
      ( if( minutes > 0 ) s"$minutes minutes " else "") +
      ( if( seconds > 0 ) s"$seconds seconds" else "")
}
