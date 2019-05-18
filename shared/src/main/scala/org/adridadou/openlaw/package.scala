package org.adridadou

import java.time.LocalDateTime

import org.adridadou.openlaw.parser.template.AgreementElement

import scala.language.implicitConversions

package object openlaw {

  trait OpenlawValue {
    type T
    val underlying: T
  }

  trait OpenlawNativeValue extends OpenlawValue {
    type T = this.type
    val underlying: T = this
  }

  object OpenlawString {
    def unapply(string: OpenlawString): Option[String] = Some(string.underlying)
  }

  object OpenlawBigDecimal {
    def unapply(bigDecimal: OpenlawBigDecimal): Option[BigDecimal] = Some(bigDecimal.underlying)
  }

  object OpenlawBoolean {
    def unapply(boolean: OpenlawBoolean): Option[Boolean] = Some(boolean.underlying)
  }

  implicit def unwrap(string: OpenlawString): String = string.underlying
  implicit def unwrap(boolean: OpenlawBoolean): Boolean = boolean.underlying
  implicit def unwrap(bigDecimal: OpenlawBigDecimal): BigDecimal = bigDecimal.underlying
  implicit def unwrap(int: OpenlawInt): Int = int.underlying

  implicit class OpenlawBoolean(override val underlying: Boolean) extends Comparable[OpenlawBoolean] with OpenlawValue {
    override type T = Boolean
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawBoolean): Int = underlying.compareTo(t.underlying)
  }

  implicit class OpenlawString(override val underlying: String) extends Comparable[OpenlawString] with OpenlawValue {
    override type T = String
    override def toString: String = underlying
    override def compareTo(t: OpenlawString): Int = underlying.compareTo(t.underlying)
  }

  implicit class OpenlawInt(override val underlying: Int) extends Comparable[OpenlawInt] with OpenlawValue {
    override type T = Int
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawInt): Int = underlying.compareTo(t.underlying)
  }

  implicit class OpenlawBigDecimal(override val underlying: BigDecimal) extends Comparable[OpenlawBigDecimal] with OpenlawValue {
    override type T = BigDecimal
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawBigDecimal): Int = underlying.compareTo(t.underlying)
  }

  implicit class OpenlawDateTime(override val underlying: LocalDateTime) extends Comparable[OpenlawDateTime] with OpenlawValue {
    override type T = LocalDateTime
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawDateTime): Int = underlying.compareTo(t.underlying)
  }

  implicit class OpenlawMap[U, Y <: OpenlawValue](override val underlying: Map[U, Y]) extends Comparable[Map[U, Y]] with OpenlawValue {
    override type T = Map[U, Y]
    override def toString: String = underlying.toString
    override def compareTo(t: Map[U, Y]): Int = underlying.compareTo(t.underlying)
  }

  // TODO: The AbstractStructure handling should be refactored to not require returning this type in access
  implicit class OpenlawElements(override val underlying: Seq[AgreementElement]) extends Comparable[Seq[AgreementElement]] with OpenlawValue {
    override type T = Seq[AgreementElement]
    override def toString: String = underlying.toString
    override def compareTo(t: Seq[AgreementElement]): Int = underlying.compareTo(t.underlying)
  }
}
