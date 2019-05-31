package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

object SignatureRSVParameter {
  implicit val signatureRSVParameterEnc: Encoder[SignatureRSVParameter] = deriveEncoder[SignatureRSVParameter]
  implicit val signatureRSVParameterDec: Decoder[SignatureRSVParameter] = deriveDecoder[SignatureRSVParameter]
}

case class SignatureRSVParameterNames(r:String, s:String, v:String) {
  def toSeq:Seq[String] = Seq(r,s,v)
}

case class SignatureRSVParameter(rExpr:Expression, sExpr:Expression, vExpr:Expression) {
  def getRsv(executionResult: TemplateExecutionResult): Result[Option[SignatureRSVParameterNames]] =
    (for {
      rOpt <- rExpr.evaluate(executionResult)
      sOpt <- sExpr.evaluate(executionResult)
      vOpt <- vExpr.evaluate(executionResult)
    } yield {
      for {
        r <- rOpt
        s <- sOpt
        v <- vOpt
      } yield {
        for {
          rString <- VariableType.convert[OpenlawString](r)
          sString <- VariableType.convert[OpenlawString](s)
          vString <- VariableType.convert[OpenlawString](v)
        } yield SignatureRSVParameterNames(rString, sString, vString)
      }
    }).map(_.sequence).flatten
}

case object EthereumCallType extends VariableType("EthereumCall") with ActionType {
  case class EthereumCallPropertyDef(typeDef:VariableType, data:Seq[EthereumSmartContractExecution] => Option[OpenlawValue])

  private val propertyDef:Map[String,EthereumCallPropertyDef] = Map[String, EthereumCallPropertyDef](
    "isSuccessful" -> EthereumCallPropertyDef(typeDef = YesNoType, _.headOption.map(_.executionStatus === SuccessfulExecution)),
    "isFailure" -> EthereumCallPropertyDef(typeDef = YesNoType, _.headOption.map(_.executionStatus === FailedExecution)),
    "executionDate" -> EthereumCallPropertyDef(typeDef = DateTimeType, _.headOption.map(_.executionDate)),
    "tx" -> EthereumCallPropertyDef(typeDef = EthTxHashType, _.headOption.map(_.tx))
  )

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[EthereumSmartContractCall] =
    handleEither(decode[EthereumSmartContractCall](value))

  override def internalFormat(value: OpenlawValue): Result[String] = value match {
    case call:EthereumSmartContractCall => Success(call.asJson.noSpaces)
  }

  override def keysType(keys: Seq[String], expression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] = {
    keys.toList match {
      case Nil => Success(thisType)
      case prop::Nil => propertyDef.get(prop) match {
        case Some(propDef) => Success(propDef.typeDef)
        case None => Failure(s"unknown property $prop for EthereumCall type")
      }
      case _ =>
        Failure(s"unknown property ${keys.mkString(".")} for EthereumCall type")
    }
  }

  override def access(value: OpenlawValue, name: VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    keys.toList match {
      case Nil => Success(Some(value))
      case prop::Nil => propertyDef.get(prop) match {
        case Some(propDef) =>
            executionResult
              .executions
              .get(name)
              .map(_.executionMap.values.toList)
              .getOrElse(List.empty)
              .map(VariableType.convert[EthereumSmartContractExecution])
              .sequence
              .map(seq => propDef.data(seq))
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
        val optNetwork = getParameter(values, "network").map(getExpression).getOrElse(NoopConstant(TextType))
        attempt(Some(EthereumSmartContractCall(
          address = getExpression(values, "contract", "to"),
          abi = getExpression(values, "interface", "abi"),
          network = optNetwork,
          signatureParameter = getParameter(values, "Signature parameter").map(getExpression),
          signatureRSVParameter = getParameter(values, "Signature RSV parameter").map(getSignatureRSVParameter),
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

  private def getSignatureRSVParameter(param:Parameter):SignatureRSVParameter = param match {
    case Parameters(parameterMap) =>
      val map = parameterMap.toMap
      SignatureRSVParameter(
        rExpr = getExpression(map, "r"),
        sExpr = getExpression(map, "s"),
        vExpr = getExpression(map, "v")
      )
    case _ => throw new RuntimeException("invalid parameter type " + param.getClass.getSimpleName + " expecting single expression")
  }

  override def getTypeClass: Class[_ <: EthereumSmartContractCall] = classOf[EthereumSmartContractCall]

  def thisType: VariableType = EthereumCallType

  private def getExpressionList(param:Parameter):Seq[Expression] = param match {
    case ListParameter(exprs) => exprs
    case OneValueParameter(expr) => Seq(expr)
    case _ => throw new RuntimeException("invalid parameter type " + param.getClass.getSimpleName + " expecting list of expressions")
  }

  override def actionValue(value: OpenlawValue): Result[EthereumSmartContractCall] = VariableType.convert[EthereumSmartContractCall](value)
}
