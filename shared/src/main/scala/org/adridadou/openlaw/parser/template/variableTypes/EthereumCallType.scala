package org.adridadou.openlaw.parser.template.variableTypes

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, attempt}

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

  override def construct(constructorParams:Parameter, executionResult: TemplateExecutionResult): Result[Option[EthereumSmartContractCall]] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        val optNetwork = getParameter(values, "network").map(getExpression).getOrElse(NoopConstant(TextType))
        attempt(Some(EthereumSmartContractCall(
          address = getExpression(values, "contract", "to"),
          metadata = getExpression(values, "interface", "abi"),
          network = optNetwork,
          functionName = getExpression(values, "function"),
          arguments = getParameter(values, "arguments", "parameters", "params").map(getExpressionList).getOrElse(Seq()),
          startDate = values.get("startDate").map(name => getExpression(name)),
          endDate = getParameter(values, "endDate").map(getExpression),
          every = getParameter(values, "repeatEvery").map(getExpression),
          from = getParameter(values, "from").map(getExpression)
        )))
      case _ =>
        Failure("Ethereum Calls need to get 'contract', 'interface', 'network', 'function' as constructor parameter")
    }
  }

  override def getTypeClass: Class[_ <: EthereumSmartContractCall] = classOf[EthereumSmartContractCall]

  def thisType: VariableType = EthereumCallType

  private def getExpressionList(param:Parameter):Seq[Expression] = param match {
    case ListParameter(exprs) => exprs
    case OneValueParameter(expr) => Seq(expr)
    case _ => throw new RuntimeException("invalid parameter type " + param.getClass.getSimpleName + " expecting list of expressions")
  }

  override def actionValue(value: Any): EthereumSmartContractCall = VariableType.convert[EthereumSmartContractCall](value)
}
