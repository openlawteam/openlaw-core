package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template.{Parameter, TemplateExecutionResult}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case object EthTxHashType extends VariableType("EthTxHash") {
  override def cast(value: String, executionResult:TemplateExecutionResult): EthereumHash = EthereumHash(value)

  override def internalFormat(value: Any): String = VariableType.convert[EthereumHash](value).withLeading0x

  override def construct(constructorParams: Parameter, executionResult:TemplateExecutionResult): Result[Option[EthereumHash]] = {
    getSingleParameter(constructorParams).evaluate(executionResult).map({
      case value:String => attempt(Some(EthereumHash(value)))
      case value:EthereumHash => Success(Some(value))
      case value => Failure("wrong type " + value.getClass.getSimpleName + ". expecting String")
    }).getOrElse(Success(None))
  }

  override def getTypeClass: Class[EthereumHash] = classOf[EthereumHash]

  def thisType: VariableType = EthTxHashType
}