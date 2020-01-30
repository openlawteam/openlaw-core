package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{
  Formatter,
  NoopFormatter
}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case object SmartContractMetadataType
    extends VariableType("SmartContractMetadata") {

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[SmartContractMetadata] = {
    val firstIndex = value.indexOf(":")
    val protocol = value.substring(0, firstIndex)
    val address = value.substring(firstIndex + 1)
    Success(SmartContractMetadata(protocol, address))
  }

  override def internalFormat(value: OpenlawValue): Result[String] =
    for {
      metadata <- VariableType.convert[SmartContractMetadata](value)
    } yield metadata.protocol + ":" + metadata.address

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[SmartContractMetadata]] = constructorParams match {
    case OneValueParameter(expr) =>
      expr
        .evaluate(executionResult)
        .flatMap(result =>
          result.map(value => cast(value.toString, executionResult)).sequence
        )
    case Parameters(v) =>
      val values = v.toMap

      (for {
        protocolParameter <- VariableType
          .convert[OneValueParameter](values("protocol"))
        optProtocol <- protocolParameter.expr.evaluate(executionResult)
        addressParameter <- VariableType
          .convert[OneValueParameter](values("address"))
        optAddress <- addressParameter.expr.evaluate(executionResult)
      } yield {
        combine(optProtocol, optAddress) {
          case (protocol, address) =>
            for {
              protocolString <- VariableType.convert[OpenlawString](protocol)
              addressString <- VariableType.convert[OpenlawString](address)
            } yield SmartContractMetadata(protocolString, addressString)
        }
      }).flatten
    case _ =>
      Failure("Smart contract metadata can be constructed with a string only")
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[SmartContractMetadata] =
    classOf[SmartContractMetadata]

  def thisType: VariableType = SmartContractMetadataType
}
