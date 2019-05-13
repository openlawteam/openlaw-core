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

  implicit class OpenlawBoolean(val boolean: Boolean) extends Comparable[OpenlawBoolean] with OpenlawValue {
    override def toString: String = boolean.toString
    override def compareTo(t: OpenlawBoolean): Int = boolean.compareTo(t.boolean)
  }

  implicit class OpenlawString(val string: String) extends Comparable[OpenlawString] with OpenlawValue {
    override def toString: String = string
    override def compareTo(t: OpenlawString): Int = string.compareTo(t.string)
  }

  implicit class OpenlawInt(val int: Int) extends Comparable[OpenlawInt] with OpenlawValue {
    override def toString: String = int.toString
    override def compareTo(t: OpenlawInt): Int = int.compareTo(t.int)
  }

  implicit class OpenlawBigDecimal(val bigDecimal: BigDecimal) extends Comparable[OpenlawBigDecimal] with OpenlawValue {
    override def toString: String = bigDecimal.toString
    override def compareTo(t: OpenlawBigDecimal): Int = bigDecimal.compareTo(t.bigDecimal)
  }

  implicit class OpenlawDateTime(val localDateTime: LocalDateTime) extends Comparable[OpenlawDateTime] with OpenlawValue {
    override def toString: String = localDateTime.toString
    override def compareTo(t: OpenlawDateTime): Int = localDateTime.compareTo(t.localDateTime)
  }

  implicit class OpenlawMap[T, Y <: OpenlawValue](val map: Map[T, Y]) extends OpenlawValue

  implicit class OpenlawElements(val seq: Seq[AgreementElement]) extends OpenlawValue // TODO: The AbstractStructure handling should be refactored to not require returning this type in access
}
