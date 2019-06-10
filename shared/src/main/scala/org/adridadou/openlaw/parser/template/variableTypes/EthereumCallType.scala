package org.adridadou.openlaw.parser.template.variableTypes

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._
import cats.implicits._
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
  def getRsv(executionResult: TemplateExecutionResult):Option[SignatureRSVParameterNames] = for {
    r <- rExpr.evaluate(executionResult)
    s <- sExpr.evaluate(executionResult)
    v <- vExpr.evaluate(executionResult)
  } yield SignatureRSVParameterNames(VariableType.convert[OpenlawString](r),VariableType.convert[OpenlawString](s),VariableType.convert[OpenlawString](v))
}

case object EthereumCallType extends VariableType("EthereumCall") with ActionType {
  case class EthereumCallPropertyDef(typeDef:VariableType, data:Seq[EthereumSmartContractExecution] => Option[OpenlawValue])

  private val propertyDef:Map[String,EthereumCallPropertyDef] = Map[String, EthereumCallPropertyDef](
    "isSuccessful" -> EthereumCallPropertyDef(typeDef = YesNoType, _.headOption.map(_.executionStatus === SuccessfulExecution)),
    "isFailure" -> EthereumCallPropertyDef(typeDef = YesNoType, _.headOption.map(_.executionStatus === FailedExecution)),
    "executionDate" -> EthereumCallPropertyDef(typeDef = DateTimeType, _.headOption.map(_.executionDate)),
    "tx" -> EthereumCallPropertyDef(typeDef = EthTxHashType, _.headOption.map(_.tx))
  )

  override def cast(value: String, executionResult: TemplateExecutionResult): EthereumSmartContractCall =
    handleEither(decode[EthereumSmartContractCall](value))

  override def internalFormat(value: OpenlawValue): String = value match {
    case call:EthereumSmartContractCall =>
      call.asJson.noSpaces
  }

  override def validateKeys(variableName:VariableName, keys:Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] =
    keys.headOption
      .map(checkIfHasProperty)
      .getOrElse(Success.unit)

  private def checkIfHasProperty(key:String):Result[Unit] =
    propertyDef.get(key).map(_ => Success.unit)
      .getOrElse(Failure(s"Ethereum Call type does not have the property $key defined"))

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
          val identifier = VariableType.convert[EthereumSmartContractCall](value).identifier(executionResult)
          val executions:Seq[EthereumSmartContractExecution] = executionResult.executions.get(identifier).map(_.executionMap.values.toSeq)
            .getOrElse(Seq())
            .map(VariableType.convert[EthereumSmartContractExecution])

          Success(propDef.data(executions))
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

  override def actionValue(value: OpenlawValue): EthereumSmartContractCall = VariableType.convert[EthereumSmartContractCall](value)
}
