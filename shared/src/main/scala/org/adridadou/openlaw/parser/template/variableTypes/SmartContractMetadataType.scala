package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw._
import org.adridadou.openlaw.OneValueParameterOpenlawValue
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, attempt}

case object SmartContractMetadataType extends VariableType("SmartContractMetadata") {

  override def cast(value: String, executionResult:TemplateExecutionResult): SmartContractMetadataOpenlawValue = {
    val firstIndex = value.indexOf(":")
    val protocol = value.substring(0,firstIndex)
    val address = value.substring(firstIndex + 1)
    SmartContractMetadata(protocol, address)
  }

  override def internalFormat(value: OpenlawValue): String = VariableType.convert[SmartContractMetadataOpenlawValue](value).get.protocol + ":" + VariableType.convert[SmartContractMetadataOpenlawValue](value).get.address

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[SmartContractMetadataOpenlawValue]] = constructorParams match {
      case OneValueParameter(expr) =>
        attempt(expr.evaluate(executionResult).map(result => cast(result.toString, executionResult).get))
      case Parameters(v) =>
        val values = v.toMap
        for {
          optProtocol <- attempt(VariableType.convert[OneValueParameterOpenlawValue](values("protocol")).get.expr.evaluate(executionResult))
          optAddress <- attempt(VariableType.convert[OneValueParameterOpenlawValue](values("address")).get.expr.evaluate(executionResult))
        } yield {
          for {
            protocol <- optProtocol
            address <- optAddress
          } yield SmartContractMetadata(VariableType.convert[String](protocol), VariableType.convert[String](address))
        }
      case _ =>
        Failure("Smart contract metadata can be constructed with a string only")
    }


  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[_ <: SmartContractMetadataType.type ] = this.getClass

  def thisType: VariableType = SmartContractMetadataType
}
