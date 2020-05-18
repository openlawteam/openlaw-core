package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, SignatureFormatter}
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}
import org.adridadou.openlaw.result.Implicits._
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression

case object ExternalSignatureType extends VariableType("ExternalSignature") {

  final case class PropertyDef(
      typeDef: VariableType,
      data: Seq[ExternalCallExecution] => Option[OpenlawValue]
  )
  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[ExternalSignature] =
    decode[ExternalSignature](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] =
    value match {
      case externalSignature: ExternalSignature =>
        Success(externalSignature.asJson.noSpaces)
      case _ =>
        Failure(
          s"expecting External Signature. got ${value.getClass.getSimpleName} instead"
        )
    }

  override def defaultFormatter: Formatter = new SignatureFormatter

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[ExternalSignature]] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        for {
          serviceNameExp <- getExpression(
            values,
            "serviceName",
            "service",
            "name",
            "service name"
          )
          strValue <- serviceNameExp.evaluateT[OpenlawString](executionResult)
          serviceName <- strValue.toResult(
            "Missing 'serviceName' property for ExternalSignature"
          )
          serviceDef <- getIntegratedService(serviceNameExp, executionResult)
          _ <- serviceDef.toResult(
            "Invalid 'serviceName' property for ExternalSignature"
          )
        } yield {
          Some(
            ExternalSignature(
              serviceName = ServiceName(serviceName.underlying),
              identity = None
            )
          )
        }
      case _ =>
        Failure(
          "ExternalSignature needs to get 'serviceName' and 'arguments' as constructor parameters"
        )
    }
  }

  override def getTypeClass: Class[_ <: ExternalSignature] =
    classOf[ExternalSignature]

  def thisType: VariableType = ExternalSignatureType

  private def getIntegratedService(
      serviceNameExp: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[IntegratedServiceDefinition]] =
    serviceNameExp.evaluateT[OpenlawString](executionResult).map { option =>
      option
        .map(ServiceName(_))
        .flatMap(executionResult.externalCallStructures.get)
    }
}

object ExternalSignature {
  implicit val externalSignatureEnc: Encoder[ExternalSignature] = deriveEncoder
  implicit val externalSignatureDec: Decoder[ExternalSignature] = deriveDecoder
}

final case class ExternalSignature(
    identity: Option[Identity] = None,
    serviceName: ServiceName
) extends OpenlawNativeValue
