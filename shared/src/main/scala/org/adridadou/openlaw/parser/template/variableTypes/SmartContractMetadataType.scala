package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}

import scala.util.Try

case object SmartContractMetadataType extends VariableType("SmartContractMetadata") {

  override def cast(value: String, executionResult:TemplateExecutionResult): SmartContractMetadata = {
    val firstIndex = value.indexOf(":")
    val protocol = value.substring(0,firstIndex)
    val address = value.substring(firstIndex + 1)
    SmartContractMetadata(protocol, address)
  }

  override def internalFormat(value: Any): String = VariableType.convert[SmartContractMetadata](value).protocol + ":" + VariableType.convert[SmartContractMetadata](value).address

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Either[Throwable, Option[SmartContractMetadata]] = constructorParams match {
      case OneValueParameter(expr) =>
        Try(expr.evaluate(executionResult).map(result => cast(result.toString, executionResult))).toEither
      case Parameters(v) =>
        val values = v.toMap
        for {
          protocol <- Try(VariableType.convert[OneValueParameter](values("protocol")).expr.evaluate(executionResult)).toEither
          address <- Try(VariableType.convert[OneValueParameter](values("address")).expr.evaluate(executionResult)).toEither
        } yield Some(SmartContractMetadata(protocol.toString, address.toString))
      case _ =>
        Left(new Exception("Smart contract metadata can be constructed with a string only"))
    }


  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[_ <: SmartContractMetadataType.type ] = this.getClass

  def thisType: VariableType = SmartContractMetadataType
}
