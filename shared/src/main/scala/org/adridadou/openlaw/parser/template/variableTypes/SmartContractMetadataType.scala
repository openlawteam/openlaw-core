package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}

case object SmartContractMetadataType extends VariableType("SmartContractMetadata") {

  override def cast(value: String, executionResult:TemplateExecutionResult): SmartContractMetadata = {
    val firstIndex = value.indexOf(":")
    val protocol = value.substring(0,firstIndex)
    val address = value.substring(firstIndex + 1)
    SmartContractMetadata(protocol, address)
  }

  override def internalFormat(value: Any): String = VariableType.convert[SmartContractMetadata](value).protocol + ":" + VariableType.convert[SmartContractMetadata](value).address

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Option[SmartContractMetadata] = constructorParams match {
      case OneValueParameter(expr) => expr.evaluate(executionResult).map(result => cast(result.toString, executionResult))
      case Parameters(v) =>
        val values = v.toMap
        for(
          protocol <- VariableType.convert[OneValueParameter](values("protocol")).expr.evaluate(executionResult);
          address <- VariableType.convert[OneValueParameter](values("address")).expr.evaluate(executionResult)
        ) yield SmartContractMetadata(protocol.toString, address.toString)
      case _ => throw new RuntimeException("Smart contract metadata can be constructed with a string only")
    }


  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[_ <: SmartContractMetadataType.type ] = this.getClass

  def thisType: VariableType = SmartContractMetadataType
}
