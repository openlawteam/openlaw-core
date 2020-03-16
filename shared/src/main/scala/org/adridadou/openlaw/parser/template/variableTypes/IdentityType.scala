package org.adridadou.openlaw.parser.template.variableTypes

import java.time.Instant

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._
import Identity._
import cats.Eq
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.{
  Formatter,
  NoopFormatter,
  SignatureFormatter
}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, Result, Success}

case object IdentityType extends VariableType(name = "Identity") {

  val identityTypes: Seq[VariableType] =
    Seq(IdentityType, ExternalSignatureType)

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[Identity] =
    decode[Identity](value) match {
      case Right(identity) => Success(identity)
      case Left(_)         => Email(value).map(Identity(_))
    }

  override def defaultFormatter: Formatter = new NoopFormatter

  def thisType: VariableType = IdentityType

  override def getTypeClass: Class[_ <: Identity] = classOf[Identity]

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    constructorParams match {
      case OneValueParameter(expr) =>
        expr
          .evaluateT[OpenlawString](executionResult)
          .flatMap(optValue =>
            optValue
              .map(Email(_).map(e => Some(Identity(e))))
              .getOrElse(Success(None))
          )
      case _ => super.construct(constructorParams, executionResult)
    }

  override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType.convert[Identity](value).map(_.asJson.noSpaces)

  override def getFormatter(
      formatterDefinition: FormatterDefinition,
      executionResult: TemplateExecutionResult
  ): Result[Formatter] = formatterDefinition.name.trim().toLowerCase() match {
    case "signature" => Success(new SignatureFormatter)
    case _           => Failure(s"unknown formatter $name")
  }

  override def access(
      value: OpenlawValue,
      name: VariableName,
      keys: List[VariableMemberKey],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    keys match {
      case Nil => Success(Some(value))
      case VariableMemberKey(Left(VariableName(head))) :: Nil =>
        VariableType
          .convert[Identity](value)
          .flatMap(id => accessProperty(Some(id), head).map(Some(_)))
      case _ :: _ =>
        Failure(
          s"Identity has only one level of properties. invalid property access ${keys.mkString(".")}"
        )
    }

  override def validateKeys(
      name: VariableName,
      keys: List[VariableMemberKey],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] = keys match {
    case Nil => Success.unit
    case VariableMemberKey(Left(VariableName(head))) :: tail if tail.isEmpty =>
      checkProperty(head)
    case _ :: _ => Failure(s"invalid property ${keys.mkString(".")}")
  }

  private def checkProperty(key: String): Result[Unit] =
    accessProperty(None, key) match {
      case Left(ex) => Failure(ex)
      case Right(_) => Success.unit
    }

  private def accessProperty(
      identity: Option[Identity],
      property: String
  ): Result[String] = {
    property.toLowerCase() match {
      case "email" => Success(getOrNa(identity.map(_.email.email)))
      case _       => Failure(s"property '$property' not found for type Identity")
    }
  }

  private def getOrNa(optStr: Option[String]): String =
    optStr.getOrElse("[n/a]")
}

final case class Identity(email: Email) extends OpenlawNativeValue {
  def getJsonString: String = this.asJson.noSpaces
}

case object Identity {
  def withEmail(email: Email): Identity = Identity(email = email)

  implicit val identityEnc: Encoder[Identity] = deriveEncoder
  implicit val identityDec: Decoder[Identity] = deriveDecoder
}

final case class Email(email: String) {
  override def toString: String = email
}

object Email {

  private val emailRegex =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  implicit val emailKeyEnc: KeyEncoder[Email] = (key: Email) => key.email
  implicit val emailKeyDec: KeyDecoder[Email] = (key: String) =>
    Email.validate(key).toOption

  implicit val eqEmail: Eq[Email] = Eq.fromUniversalEquals
  implicit val emailEnc: Encoder[Email] = (a: Email) => Json.fromString(a.email)
  implicit val emailDec: Decoder[Email] = (c: HCursor) =>
    c.as[String] match {
      case Right(strEmail) =>
        Email.validate(strEmail) match {
          case Right(email) =>
            Right(email)
          case Left(err) =>
            Left(DecodingFailure(err.message, Nil))
        }
      case Left(ex) =>
        Left(ex)
    }

  def validate(email: String): Result[Email] =
    emailRegex.findFirstMatchIn(email) match {
      case Some(_) => Success(new Email(email.trim.toLowerCase()))
      case None    => Failure(s"invalid Email $email")
    }

  def apply(email: String): Result[Email] = validate(email)
}

object SignatureAction {
  implicit val signatureActionEq: Eq[SignatureAction] = Eq.fromUniversalEquals
}

final case class SignatureAction(
    email: Email,
    services: List[ServiceName] = List(ServiceName.openlawServiceName)
) extends ActionValue {
  override def nextActionSchedule(
      executionResult: TemplateExecutionResult,
      pastExecutions: List[OpenlawExecution]
  ): Result[Option[Instant]] =
    if (executionResult.hasSigned(email)) {
      Success(None)
    } else {
      Success(Some(executionResult.info.now))
    }

  override def identifier(
      executionResult: TemplateExecutionResult
  ): Result[ActionIdentifier] = Success(ActionIdentifier(email.email))
}
