package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.{EthereumHashOpenlawValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.{Parameter, TemplateExecutionResult}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case object EthTxHashType extends VariableType("EthTxHash") {
  override def cast(value: String, executionResult:TemplateExecutionResult): EthereumHashOpenlawValue = EthereumHash(value)

  override def internalFormat(value: OpenlawValue): String = VariableType.convert[EthereumHashOpenlawValue](value).get.withLeading0x

  override def construct(constructorParams: Parameter, executionResult:TemplateExecutionResult): Result[Option[EthereumHashOpenlawValue]] = {
    getSingleParameter(constructorParams).evaluate(executionResult).map(_.get).map({
      case value:String => attempt(Some(EthereumHashOpenlawValue(EthereumHash(value))))
      case value:EthereumHash => Success(Some(EthereumHashOpenlawValue(value)))
      case value => Failure("wrong type " + value.getClass.getSimpleName + ". expecting String")
    }).getOrElse(Success(None))
  }

  override def getTypeClass: Class[EthereumHash] = classOf[EthereumHash]

  def thisType: VariableType = EthTxHashType
}