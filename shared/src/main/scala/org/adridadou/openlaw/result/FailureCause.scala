package org.adridadou.openlaw.result

import cats.data.NonEmptyList
import Implicits.RichNonEmptyList

final case class MultipleCauseException(causes: NonEmptyList[FailureCause]) extends java.lang.RuntimeException(causes.map(_.message).mkString)

trait FailureCause {
  def e: Exception
  def id: String
  def throwException[T](): T = throw e
  def message: String = Option(e.getMessage).getOrElse("")
}

object FailureCause {
  def unapply(result: Result[_]): Option[FailureCause] = result.swap.toOption
}

final case class FailureException(override val e: Exception, idOpt: Option[String] = None) extends FailureCause {
  override val id = idOpt.getOrElse(message)
}

final case class FailureMessage(override val message: String, idOpt: Option[String] = None) extends FailureCause {
  override val e = new RuntimeException(message)
  override val id = idOpt.getOrElse(message)
}

