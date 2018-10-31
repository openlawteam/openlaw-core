package org.adridadou.openlaw.parser.template.variableTypes

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}

case object EthereumCallType extends VariableType("EthereumCall") with ActionType {
  implicit val smartContractEnc: Encoder[EthereumSmartContractCall] = deriveEncoder[EthereumSmartContractCall]
  implicit val smartContractDec: Decoder[EthereumSmartContractCall] = deriveDecoder[EthereumSmartContractCall]

  override def cast(value: String, executionResult: TemplateExecutionResult): EthereumSmartContractCall =
    handleEither(decode[EthereumSmartContractCall](value))

  override def internalFormat(value: Any): String = value match {
    case call:EthereumSmartContractCall =>
      call.asJson.noSpaces
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def construct(constructorParams:Parameter, executionResult: TemplateExecutionResult): Option[EthereumSmartContractCall] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        val optNetwork = values.get("network").map(getExpression).getOrElse(NoopConstant(TextType))
        Some(EthereumSmartContractCall(
          address = getExpression(values, "contract"),
          metadata = getExpression(values, "interface"),
          network = optNetwork,
          functionName = getExpression(values, "function"),
          arguments = values.get("arguments").map(getExpressionList).getOrElse(Seq()),
          startDate = values.get("startDate").map(name => getExpression(name)),
          endDate = values.get("endDate").map(getExpression),
          every = values.get("repeatEvery").map(getExpression)
        ))
      case _ =>
        throw new RuntimeException("Ethereum Calls need to get 'contract', 'interface', 'network', 'function' as constructor parameter")
    }
  }

  def thisType: VariableType = EthereumCallType

  private def getExpressionList(param:Parameter):Seq[Expression] = param match {
    case ListParameter(exprs) => exprs
    case OneValueParameter(expr) => Seq(expr)
    case _ => throw new RuntimeException("invalid parameter type " + param.getClass.getSimpleName + " expecting list of expressions")
  }

  override def actionValue(value: Any): EthereumSmartContractCall = VariableType.convert[EthereumSmartContractCall](value)
}
