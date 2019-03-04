package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case object SmartContractMetadataType extends VariableType("SmartContractMetadata") {

  override def cast(value: String, executionResult:TemplateExecutionResult): Result[SmartContractMetadata] = {
    val firstIndex = value.indexOf(":")
    val protocol = value.substring(0,firstIndex)
    val address = value.substring(firstIndex + 1)
    Success(SmartContractMetadata(protocol, address))
  }

  override def internalFormat(value: Any): Result[String] =
    for {
      metadata <- VariableType.convert[SmartContractMetadata](value)
    } yield metadata.protocol + ":" + metadata.address

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[SmartContractMetadata]] = constructorParams match {
      case OneValueParameter(expr) =>
        for {
          option <- expr.evaluate(executionResult).sequence
          metadata <- option.map(result => cast(result.toString, executionResult)).sequence
        } yield metadata

      case Parameters(v) =>
        val values = v.toMap
        for {
          protocolResult <- VariableType.convert[OneValueParameter](values("protocol"))
          addressResult <- VariableType.convert[OneValueParameter](values("address"))
        } yield {
          for {
            protocol <- protocolResult.expr.evaluate(executionResult)
            address <- addressResult.expr.evaluate(executionResult)
          } yield SmartContractMetadata(protocol.toString, address.toString)
        }
      case _ =>
        Failure("Smart contract metadata can be constructed with a string only")
    }


  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[_ <: SmartContractMetadataType.type ] = this.getClass

  def thisType: VariableType = SmartContractMetadataType
}
