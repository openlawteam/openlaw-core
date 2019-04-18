package org.adridadou.openlaw.parser.template.variableTypes

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}


case object EthereumCallType extends VariableType("EthereumCall") with ActionType {
  implicit val smartContractEnc: Encoder[EthereumSmartContractCall] = deriveEncoder[EthereumSmartContractCall]
  implicit val smartContractDec: Decoder[EthereumSmartContractCall] = deriveDecoder[EthereumSmartContractCall]

  case class EthereumCallPropertyDef(typeDef:VariableType, data:EthereumSmartContractExecution => Option[Any])

  private val propertyDef:Map[String,VariableType] = Map[String, VariableType](
    "isSuccessful" -> YesNoType,
    "isFailure" -> YesNoType,
    "isPending" -> YesNoType,
    "executionTime" -> DateTimeType,
    "tx" -> EthTxHashType
  )

  override def cast(value: String, executionResult: TemplateExecutionResult): EthereumSmartContractCall =
    handleEither(decode[EthereumSmartContractCall](value))

  override def internalFormat(value: Any): String = value match {
    case call:EthereumSmartContractCall =>
      call.asJson.noSpaces
  }

  override def keysType(keys: Seq[String], expression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] = {
    keys.toList match {
      case Nil => Success(thisType)
      case prop::Nil => propertyDef.get(prop) match {
        case Some(typeDef) => Success(typeDef)
        case None => Failure(s"unknown property $prop for EthereumCall type")
      }
      case _ =>
        Failure(s"unknown property ${keys.mkString(".")} for EthereumCall type")
    }
  }

  override def access(value: Any, name: VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[Any]] = {
    keys.toList match {
      case Nil => Success(Some(value))
      case prop::Nil => propertyDef.get(prop) match {
        case Some(typeDef) => Success(None)
        case None => Failure(s"unknown property $prop for EthereumCall type")
      }
      case _ =>
        Failure(s"unknown property ${keys.mkString(".")} for EthereumCall type")
    }
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def construct(constructorParams:Parameter, executionResult: TemplateExecutionResult): Result[Option[EthereumSmartContractCall]] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        val optNetwork = values.get("network").map(getExpression).getOrElse(NoopConstant(TextType))
        attempt(Some(EthereumSmartContractCall(
          address = getExpression(values, "contract"),
          metadata = getExpression(values, "interface"),
          network = optNetwork,
          functionName = getExpression(values, "function"),
          arguments = values.get("arguments").map(getExpressionList).getOrElse(Seq()),
          startDate = values.get("startDate").map(name => getExpression(name)),
          endDate = values.get("endDate").map(getExpression),
          every = values.get("repeatEvery").map(getExpression)
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
