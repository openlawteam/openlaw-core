package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, attempt}

case object SmartContractMetadataType extends VariableType("SmartContractMetadata") {

  override def cast(value: String, executionResult:TemplateExecutionResult): SmartContractMetadata = {
    val firstIndex = value.indexOf(":")
    val protocol = value.substring(0,firstIndex)
    val address = value.substring(firstIndex + 1)
    SmartContractMetadata(protocol, address)
  }

  override def internalFormat(value: OpenlawValue): String = VariableType.convert[SmartContractMetadata](value).protocol + ":" + VariableType.convert[SmartContractMetadata](value).address

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[SmartContractMetadata]] = constructorParams match {
      case OneValueParameter(expr) =>
        attempt(expr.evaluate(executionResult).map(result => cast(result.toString, executionResult)))
      case Parameters(v) =>
        val values = v.toMap
        for {
          optProtocol <- attempt(VariableType.convert[OneValueParameter](values("protocol")).expr.evaluate(executionResult))
          optAddress <- attempt(VariableType.convert[OneValueParameter](values("address")).expr.evaluate(executionResult))
        } yield {
          for {
            protocol <- optProtocol
            address <- optAddress
          } yield SmartContractMetadata(VariableType.convert[OpenlawString](protocol), VariableType.convert[OpenlawString](address))
        }
      case _ =>
        Failure("Smart contract metadata can be constructed with a string only")
    }


  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[_ <: SmartContractMetadataType.type ] = this.getClass

  def thisType: VariableType = SmartContractMetadataType
}
