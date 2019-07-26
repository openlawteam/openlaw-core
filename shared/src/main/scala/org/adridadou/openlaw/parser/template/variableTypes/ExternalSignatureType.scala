package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import cats.implicits._

case object ExternalSignatureType extends VariableType("ExternalSignature") {

  case class PropertyDef(typeDef: VariableType, data: Seq[ExternalCallExecution] => Option[OpenlawValue])
  override def cast(value: String, executionResult: TemplateExecutionResult): Result[ExternalSignature] =
    decode[ExternalSignature](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] = value match {
    case externalSignature: ExternalSignature =>
      Success(externalSignature.asJson.noSpaces)
    case _ => Failure(s"expecting External Signature. got ${value.getClass.getSimpleName} instead")
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[ExternalSignature]] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        for {
          serviceName <- getExpression(values, "serviceName", "service", "name", "service name").flatMap(_.evaluateT[OpenlawString](executionResult))
        } yield {
          Some(ExternalSignature(
            serviceName = ServiceName(serviceName.getOrElse("")),
            identity = None
          ))
        }
      case _ =>
        Failure("ExternalCall needs to get 'serviceName' and 'arguments' as constructor parameters")
    }
  }

  override def getTypeClass: Class[_ <: ExternalSignature] = classOf[ExternalSignature]

  def thisType: VariableType = ExternalSignatureType
}

object ExternalSignature {
  implicit val externalSignatureEnc:Encoder[ExternalSignature] = deriveEncoder
  implicit val externalSignatureDec:Decoder[ExternalSignature] = deriveDecoder
}

case class ExternalSignature(identity:Option[Identity] = None, serviceName: ServiceName) extends OpenlawNativeValue