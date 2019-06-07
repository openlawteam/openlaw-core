package org.adridadou.openlaw.result

import cats.implicits._
import cats.data.NonEmptyList
import cats.data.NonEmptyList.of
import cats.data.Validated.Valid
import play.api.libs.json._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.language.implicitConversions
import scala.util.{Try, Failure => TFailure, Success => TSuccess}

object Implicits {

  implicit class RichNonEmptyList[T](val nel: NonEmptyList[T]) extends AnyVal {
    def mkString: String = mkString(", ")
    def mkString(sep: String): String = nel.toList.mkString(sep)
  }

  implicit class RichJsResult[T](val j: JsResult[T]) extends AnyVal {
    def toResult: Result[T] = j.toResultNel.toResult
    def toResultNel: ResultNel[T] = j match {
      case JsSuccess(t, _) => Valid(t)
      case JsError(errors) =>
        val messages = errors.flatMap { case (path, es) => es.map(e => e.message -> path.toString) }
        messages match {
          case Seq() => Failure().toResultNel
          case Seq(x, xs @ _*) => Failure(of[(String, String)](x, xs : _*))
        }
    }
  }

  implicit class RichTry[T](val t: Try[T]) extends AnyVal {
    def toResult: Result[T] = t match {
      case TSuccess(v) => Success(v)
      case TFailure(e: Exception) => Failure(e)

      // don't try to handle Error instances
      case TFailure(t) => throw t
    }
  }

  implicit class RichEither[T](val either: Either[String, T]) extends AnyVal {
    def toResult: Result[T] = either.left.map(FailureMessage(_))
  }

  implicit class RichFuture[T](val future: Future[T]) extends AnyVal {
    def getResult(timeout: Duration): Result[T] = attempt(Await.result(future, timeout))
  }

  implicit class RichResult[T](val result: Result[T]) extends AnyVal {
    def addCause(cause: Failure[T]): ResultNel[T] = result match {
      case Success(_) => cause.toResultNel
      case Left(original) => FailureNel(original, cause.value)
    }
    def addFailure[U >: T](cause: FailureCause): ResultNel[U] = result match {
      case s @ Success(_) => s.toResultNel
      case Left(original) => FailureNel(cause, original)
    }
    def addMessageToFailure[U >: T](message: String): ResultNel[U] = result match {
      case s @ Success(_) => s.toResultNel
      case Left(original) => FailureNel(FailureMessage(message), original)
    }
    def convert(pf: PartialFunction[Exception, Exception]): Result[T] =
      result.left.map {
        case FailureException(e, _) if pf.isDefinedAt(e) => FailureException(pf(e))
        case f => f
      }
    def recoverMerge(f: FailureCause => T): T = result.fold(failure => f(failure), success => success)
    def recoverWith(pf: PartialFunction[FailureCause, Result[T]]): Result[T] = result.left.flatMap { error =>
      if (pf.isDefinedAt(error)) pf(error) else result
    }
    def toResultNel: ResultNel[T] = result.toValidatedNel
    def toFuture: Future[T] = result match {
      case Success(value) => Future.successful(value)
      case Failure(e, _) => Future.failed(e)
    }
    def getOrThrow(): T = result.valueOr(_.throwException())
  }

  implicit class RichOption[T](val option: Option[T]) extends AnyVal {
    def toResult(message: String): Result[T] = option.map(x => Success(x)).getOrElse(Failure(message))
  }

  implicit class RichResultNel[T](val result: ResultNel[T]) extends AnyVal {
    def toUnit: ResultNel[Unit] = result.map(_ => ())
    def toResult: Result[T] = result.toEither.leftMap {
      case NonEmptyList(x, Seq()) => x
      case nel => FailureException(MultipleCauseException(nel))
    }
  }

  implicit def exception2Result[A](e: Exception): Result[A] = Failure[A](e)
  implicit def unitResultConversion[T](wrapped: Result[T]): Result[Unit] = wrapped.map(_ => ())
  implicit def failureCause2Exception[T](wrapped: FailureCause): Exception = wrapped.e
}

