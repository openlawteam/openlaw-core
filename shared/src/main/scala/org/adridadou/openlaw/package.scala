package org.adridadou

import java.time.LocalDateTime
import org.adridadou.openlaw.parser.template.AgreementElement
import scala.collection.mutable
import scala.collection.JavaConverters._
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
    def unapply(value: OpenlawString): Option[String] = Some(value.underlying)
  }

  object OpenlawBoolean {
    def unapply(value: OpenlawBoolean): Option[Boolean] = Some(value.underlying)
  }

  object OpenlawBigDecimal {
    def unapply(value: OpenlawBigDecimal): Option[BigDecimal] =
      Some(value.underlying)
  }

  object OpenlawDateTime {
    def unapply(value: OpenlawDateTime): Option[LocalDateTime] =
      Some(value.underlying)
  }

  implicit def unwrap[U <: OpenlawValue](value: U): U#T = value.underlying

  implicit class OpenlawBoolean(override val underlying: Boolean)
      extends Comparable[OpenlawBoolean]
      with OpenlawValue {
    override type T = Boolean
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawBoolean): Int =
      underlying.compareTo(t.underlying)
    override def equals(o: Any): Boolean = o match {
      case value: OpenlawValue => underlying.equals(value.underlying)
      case _                   => underlying.equals(o)
    }
    override def hashCode: Int = underlying.hashCode
  }

  implicit class OpenlawString(override val underlying: String)
      extends Comparable[OpenlawString]
      with OpenlawValue {
    override type T = String
    override def toString: String = underlying
    override def compareTo(t: OpenlawString): Int =
      underlying.compareTo(t.underlying)
    override def equals(o: Any): Boolean = o match {
      case value: OpenlawValue => underlying.equals(value.underlying)
      case _                   => underlying.equals(o)
    }
    override def hashCode: Int = underlying.hashCode
  }

  implicit class OpenlawInt(override val underlying: Int)
      extends Comparable[OpenlawInt]
      with OpenlawValue {
    override type T = Int
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawInt): Int =
      underlying.compareTo(t.underlying)
    override def equals(o: Any): Boolean = o match {
      case value: OpenlawValue => underlying.equals(value.underlying)
      case _                   => underlying.equals(o)
    }
    override def hashCode: Int = underlying.hashCode
  }

  implicit class OpenlawBigDecimal(override val underlying: BigDecimal)
      extends Comparable[OpenlawBigDecimal]
      with OpenlawValue {
    override type T = BigDecimal
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawBigDecimal): Int =
      underlying.compareTo(t.underlying)
    override def equals(o: Any): Boolean = o match {
      case value: OpenlawValue => underlying.equals(value.underlying)
      case _                   => underlying.equals(o)
    }
    override def hashCode: Int = underlying.hashCode
  }

  implicit class OpenlawDateTime(override val underlying: LocalDateTime)
      extends Comparable[OpenlawDateTime]
      with OpenlawValue {
    override type T = LocalDateTime
    override def toString: String = underlying.toString
    override def compareTo(t: OpenlawDateTime): Int =
      underlying.compareTo(t.underlying)
    override def equals(o: Any): Boolean = o match {
      case value: OpenlawValue => underlying.equals(value.underlying)
      case _                   => underlying.equals(o)
    }
    override def hashCode: Int = underlying.hashCode
  }

  implicit class OpenlawMap[U, Y <: OpenlawValue](
      override val underlying: Map[U, Y]
  ) extends Comparable[Map[U, Y]]
      with OpenlawValue {
    override type T = Map[U, Y]
    override def toString: String = underlying.toString
    override def compareTo(t: Map[U, Y]): Int =
      underlying.compareTo(t.underlying)
    override def equals(o: Any): Boolean = o match {
      case value: OpenlawValue => underlying.equals(value.underlying)
      case _                   => underlying.equals(o)
    }
    override def hashCode: Int = underlying.hashCode
  }

  // TODO: The AbstractStructure handling should be refactored to not require returning this type in access
  implicit class OpenlawElements(
      override val underlying: List[AgreementElement]
  ) extends Comparable[List[AgreementElement]]
      with OpenlawValue {
    override type T = List[AgreementElement]
    override def toString: String = underlying.toString
    override def compareTo(t: List[AgreementElement]): Int =
      underlying.compareTo(t.underlying)
    override def equals(o: Any): Boolean = o match {
      case value: OpenlawValue => underlying.equals(value.underlying)
      case _                   => underlying.equals(o)
    }
    override def hashCode: Int = underlying.hashCode
  }

  // Creates a thread-safe mutable map.
  def createConcurrentMutableMap[K, V]: mutable.Map[K, V] =
    createConcurrentMutableMap(values = collection.Map.empty[K, V])

  // Creates a thread-safe mutable map containing the provided entries.
  def createConcurrentMutableMap[K, V](
      values: collection.Map[K, V]
  ): mutable.Map[K, V] =
    (new mutable.LinkedHashMap[K, V]
      with mutable.SynchronizedMap[K, V]) ++ values

  // Creates a thread-safe mutable buffer.
  def createConcurrentMutableBuffer[T]: mutable.Buffer[T] =
    createConcurrentMutableBuffer(Nil)

  // Creates a thread-safe mutable buffer containing the provided entries.
  def createConcurrentMutableBuffer[T](values: Iterable[T]): mutable.Buffer[T] =
    (new mutable.ArrayBuffer[T] with mutable.SynchronizedBuffer[T]) ++ values

  // Creates a thread-safe mutable set containing the optional provided entries.
  def createConcurrentMutableSet[T]: mutable.Set[T] =
    createConcurrentMutableSet(Nil)

  // Creates a thread-safe mutable set containing the optional provided entries.
  def createConcurrentMutableSet[T](values: Iterable[T]): mutable.Set[T] =
    (new mutable.HashSet[T] with mutable.SynchronizedSet[T]) ++ values
}
