package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.result.{Failure, Result, Success}

case object StripeCallType extends VariableType(name = "StripeCall") {
  implicit val smartContractEnc: Encoder[StripeCall] =
    deriveEncoder[StripeCall]
  implicit val smartContractDec: Decoder[StripeCall] =
    deriveDecoder[StripeCall]

  override def cast(value: String, executionResult: TemplateExecutionResult): StripeCall =
    handleEither(decode[StripeCall](value))

  override def internalFormat(value: Any): String =
    value match {
      case call: StripeCall => call.asJson.noSpaces
    }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[_ <: StripeCallType.type ] = this.getClass

  override def construct(
    constructorParams: Parameter,
    executionResult: TemplateExecutionResult
  ): Result[Option[StripeCall]] =
    constructorParams match {
      case Parameters(values) =>
        Success(Some(StripeCall(beneficiary = getExpression(values.toMap, "beneficiary"))))
      case _ =>
        Failure("Stripe Calls need to get 'beneficiary' as constructor parameter")
    }

  def thisType: VariableType = StripeCallType
}
