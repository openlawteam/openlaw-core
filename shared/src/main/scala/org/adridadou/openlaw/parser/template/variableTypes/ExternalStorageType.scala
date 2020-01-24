package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe._
import org.adridadou.openlaw.result.Implicits._
import org.adridadou.openlaw
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.{
  Parameter,
  Parameters,
  TemplateExecutionResult,
}
import org.adridadou.openlaw.result.{
  Failure,
  FailureException,
  Result,
  Success,
}
import cats.implicits._
import io.circe.parser.decode
import org.adridadou.openlaw.parser.template.expressions.Expression

final case class ExternalStorage(serviceName: Expression,
                                 fileType: Expression,
                                 filePath: Expression)
    extends OpenlawNativeValue

object ExternalStorage {
  implicit val externalCallEnc: Encoder[ExternalStorage] = deriveEncoder
  implicit val externalCallDec: Decoder[ExternalStorage] = deriveDecoder
}

object ExternalStorageType extends VariableType("ExternalStorage") {

  override def getTypeClass: Class[_ <: openlaw.OpenlawValue] =
    classOf[ExternalStorage]

  override def construct(
    constructorParams: Parameter,
    executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        for {
          serviceNameExp <- getExpression(
            values,
            "serviceName",
            "service",
            "name",
            "storage"
          )
          fileTypeExp <- getParameter(values, "fileType")
            .map(getExpression)
            .sequence
          filePathExp <- getParameter(values, "filePath")
            .map(getExpression)
            .sequence
          serviceDef <- getIntegratedService(serviceNameExp, executionResult)
          _ <- serviceDef.toResult(
            "Invalid or missing 'serviceName', 'service' or 'storage' property for ExternalStorage declaration"
          )
          fileType <- fileTypeExp.toResult(
            "Invalid or missing 'fileType' property for ExternalStorage declaration"
          )
          filePath <- filePathExp.toResult(
            "Invalid or missing 'filePath' property for ExternalStorage declaration"
          )
        } yield {
          Some(
            ExternalStorage(
              serviceName = serviceNameExp,
              fileType = fileType,
              filePath = filePath
            )
          )
        }
      case _ =>
        Failure(
          "ExternalStorage declaration needs to get 'serviceName', 'fileType' and 'filePath' as constructor parameters"
        )
    }
  }

  override def cast(
    value: String,
    executionResult: TemplateExecutionResult
  ): Result[ExternalStorage] =
    decode[ExternalStorage](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] =
    value match {
      case extStorage: ExternalStorage =>
        Success(extStorage.asJson.noSpaces)
    }

  override def thisType: VariableType = RegexType

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
