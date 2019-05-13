package org.adridadou

import java.time.LocalDateTime

import org.adridadou.openlaw.parser.template.AgreementElement

package object openlaw {

  trait OpenlawValue

  object OpenlawString {
    def unapply(string: OpenlawString): Option[String] = Some(string.string)
  }
  object OpenlawBigDecimal {
    def unapply(bigDecimal: OpenlawBigDecimal): Option[BigDecimal] = Some(bigDecimal.bigDecimal)
  }

  object OpenlawBoolean {
    def unapply(boolean: OpenlawBoolean): Option[Boolean] = Some(boolean.boolean)
  }

  implicit def unwrap(string: OpenlawString): String = string.string
  implicit def unwrap(boolean: OpenlawBoolean): Boolean = boolean.boolean
  implicit def unwrap(bigDecimal: OpenlawBigDecimal): BigDecimal = bigDecimal.bigDecimal
  implicit def unwrap(int: OpenlawInt): Int = int.int

  implicit class OpenlawBoolean(val boolean: Boolean) extends OpenlawValue
  implicit class OpenlawString(val string: String) extends OpenlawValue
  implicit class OpenlawInt(val int: Int) extends OpenlawValue
  implicit class OpenlawInteger(val int: Integer) extends OpenlawValue
  implicit class OpenlawBigDecimal(val bigDecimal: BigDecimal) extends OpenlawValue
  implicit class OpenlawDateTime(val localDateTime: LocalDateTime) extends OpenlawValue
  implicit class OpenlawMap[T, Y <: OpenlawValue](val map: Map[T, Y]) extends OpenlawValue
  implicit class OpenlawElements(val seq: Seq[AgreementElement]) extends OpenlawValue
}
