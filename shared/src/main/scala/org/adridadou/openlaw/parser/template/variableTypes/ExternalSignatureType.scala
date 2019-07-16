package org.adridadou.openlaw.parser.template.variableTypes

import java.time.LocalDateTime

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}

case object ExternalSignatureType extends VariableType("ExternalSignature") with ActionType {

  case class PropertyDef(typeDef: VariableType, data: Seq[ExternalCallExecution] => Option[OpenlawValue])
  override def cast(value: String, executionResult: TemplateExecutionResult): Result[ExternalSignature] =
    handleEither(decode[ExternalSignature](value))

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

  override def actionValue(value: OpenlawValue): Result[ExternalSignature] = VariableType.convert[ExternalSignature](value)
}

object ExternalSignature {
  implicit val externalSignatureEnc:Encoder[ExternalSignature] = deriveEncoder
  implicit val externalSignatureDec:Decoder[ExternalSignature] = deriveDecoder
}

case class ExternalSignature(identity:Option[Identity] = None, serviceName: ServiceName) extends ActionValue with OpenlawNativeValue {

  override def identifier(executionResult: TemplateExecutionResult): Result[ActionIdentifier] = identity match {
    case Some(id) => Success(ActionIdentifier(id.email.email))
    case None => Success(ActionIdentifier("undefined"))
  }

  override def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions: Seq[OpenlawExecution]): Result[Option[LocalDateTime]] = {
    identity match {
      case Some(id) if executionResult.hasSigned(id.email) => Success(None)
      case Some(_) => Success(Some(LocalDateTime.now))
      case None => Success(Some(LocalDateTime.now))
    }
  }

}