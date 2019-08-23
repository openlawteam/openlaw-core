package org.adridadou.openlaw

import result.Implicits.RichResult
import cats.data.Validated._
import cats.data.{NonEmptyList, ValidatedNel}
import cats.data.NonEmptyList.{of, one}
import cats.implicits._
import cats.Semigroup
import org.adridadou.openlaw.result.Implicits.RichTry
import scala.util.Try
import scala.util.control.NonFatal

package object result {

  type ResultNel[+A] = ValidatedNel[FailureCause, A]

  type Result[+A] = Either[FailureCause, A]
  type Failure[Nothing] = Left[FailureCause, Nothing]
  type Success[+A] = Right[FailureCause, A]

  def attempt[A](f: => A): Result[A] = Try(f).toResult

  def handleFatalErrors(t: Throwable): Result[Nothing] = t match {
    case NonFatal(e: Exception) => Failure(e)
    case e => throw e
  }
}

package result {

  object Success {
    def unit:Result[Unit] = Success(())
    def apply[A](a: A): Result[A] = Right(a)
    def unapply[A](result: Result[A]): Option[A] = result.toOption
  }

  object ResultNel {
    def apply[A](nel: NonEmptyList[Result[A]]): ResultNel[NonEmptyList[A]] = nel.map(_.toResultNel).sequence
    def apply[A](nel: List[Result[A]]): ResultNel[List[A]] = nel.map(_.toResultNel).sequence
  }

  object FailureNel {
    def apply[A](e: FailureCause): ResultNel[A] = Invalid[NonEmptyList[FailureCause]](one(e))
    def apply[A](head: FailureCause, tail: FailureCause*): ResultNel[A] = Invalid[NonEmptyList[FailureCause]](of(head, tail : _*))
  }

  object Failure {
    def apply[A](f: FailureCause): Result[A] = Left(f)
    def apply[A](): Result[A] = apply(new RuntimeException)
    def apply[A](e: Exception): Result[A] = apply(FailureException(e))
    def apply[A](e: Exception, id: String): Result[A] = apply(FailureException(e, Some(id)))
    def apply[A](message: String): Result[A] = apply(FailureMessage(message))
    def apply[A](message: String, id: String): Result[A] = apply(FailureMessage(message, Some(id)))
    def apply[A](es: NonEmptyList[Exception]): ResultNel[A] = Invalid(es.map(FailureException(_)))
    // implicits are necessary here to disambiguate arguments after erasure
    def apply[A](es: NonEmptyList[(Exception, String)])(implicit i: DummyImplicit): ResultNel[A] =
      Invalid(es.map { case (e, id) => FailureException(e, Some(id)) })
    def apply[A](messages: NonEmptyList[String])(implicit i: DummyImplicit, i2: DummyImplicit): ResultNel[A] =
      Invalid(messages.map(FailureMessage(_)))
    def apply[A](messages: NonEmptyList[(String, String)])(implicit i: DummyImplicit, i2: DummyImplicit, i3: DummyImplicit): ResultNel[A] =
      Invalid(messages.map { case (m, id) => FailureMessage(m, Some(id)) })
    def unapply(result: Result[_]): Option[(Exception, String)] = result.swap.toOption.map { f => (f.e, f.id) }
  }
}

