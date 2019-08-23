package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._
import cats.implicits._
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success, attempt}

object SignatureRSVParameter {
  implicit val signatureRSVParameterEnc: Encoder[SignatureRSVParameter] = deriveEncoder
  implicit val signatureRSVParameterDec: Decoder[SignatureRSVParameter] = deriveDecoder
}

final case class SignatureRSVParameterNames(r:String, s:String, v:String) {
  def toSeq:Seq[String] = Seq(r,s,v)
}

final case class SignatureRSVParameter(rExpr:Expression, sExpr:Expression, vExpr:Expression) {
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
  final case class EthereumCallPropertyDef(typeDef:VariableType, data:Seq[EthereumSmartContractExecution] => Option[OpenlawValue])

  private val propertyDef:Map[String,EthereumCallPropertyDef] = Map[String, EthereumCallPropertyDef](
    "isSuccessful" -> EthereumCallPropertyDef(typeDef = YesNoType, _.headOption.map(_.executionStatus === SuccessfulExecution)),
    "isFailure" -> EthereumCallPropertyDef(typeDef = YesNoType, _.headOption.map(_.executionStatus === FailedExecution)),
    "executionDate" -> EthereumCallPropertyDef(typeDef = DateTimeType, _.headOption.map(_.executionDate)),
    "tx" -> EthereumCallPropertyDef(typeDef = EthTxHashType, _.headOption.map(_.tx))
  )

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[EthereumSmartContractCall] =
    decode[EthereumSmartContractCall](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] = value match {
    case call:EthereumSmartContractCall => Success(call.asJson.noSpaces)
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
          (for {
            call <- VariableType.convert[EthereumSmartContractCall](value)
            identifier <- call.identifier(executionResult)
          } yield {
            executionResult
              .executions
              .get(identifier)
              .map(_.executionMap.values.toList)
              .getOrElse(List.empty)
              .map(VariableType.convert[EthereumSmartContractExecution])
              .sequence
              .map(seq => propDef.data(seq))
          }).flatten
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
        for {
          address <- getExpression(values, "contract", "to")
          abi <- getExpression(values, "interface", "abi")
          optNetwork <- getParameter(values, "network").map(getExpression).sequence.map(_.getOrElse(NoopConstant(TextType)))
          signatureParameter <- getParameter(values, "Signature parameter").map(getExpression).sequence
          signatureRSVParameter <- getParameter(values, "Signature RSV parameter").map(getSignatureRSVParameter).sequence
          functionName <- getExpression(values, "function")
          expressionList <- getParameter(values, "arguments", "parameters", "params").map(getExpressionList).sequence.map(_.getOrElse(Seq()))
          startDate <- values.get("startDate").map(name => getExpression(name)).sequence
          endDate <- getParameter(values, "endDate").map(getExpression).sequence
          every <- getParameter(values, "repeatEvery").map(getExpression).sequence
          from <- getParameter(values, "from").map(getExpression).sequence
        } yield
          Some(EthereumSmartContractCall(
            address = address,
            abi = abi,
            network = optNetwork,
            signatureParameter = signatureParameter,
            signatureRSVParameter = signatureRSVParameter,
            functionName = functionName,
            arguments = expressionList,
            startDate = startDate,
            endDate = endDate,
            every = every,
            from = from
          ))
      case _ =>
        Failure("Ethereum Calls need to get 'contract', 'interface', 'network', 'function' as constructor parameter")
    }
  }

  private def getSignatureRSVParameter(param:Parameter): Result[SignatureRSVParameter] = param match {
    case Parameters(parameterMap) =>
      val map = parameterMap.toMap
      for {
        rExpr <- getExpression(map, "r")
        sExpr <- getExpression(map, "s")
        vExpr <- getExpression(map, "v")
      } yield SignatureRSVParameter(rExpr = rExpr, sExpr = sExpr, vExpr = vExpr)
    case _ => Failure("invalid parameter type " + param.getClass.getSimpleName + " expecting single expression")
  }

  override def getTypeClass: Class[_ <: EthereumSmartContractCall] = classOf[EthereumSmartContractCall]

  def thisType: VariableType = EthereumCallType

  private def getExpressionList(param:Parameter): Result[Seq[Expression]] = param match {
    case ListParameter(exprs) => Success(exprs)
    case OneValueParameter(expr) => Success(Seq(expr))
    case _ => Failure("invalid parameter type " + param.getClass.getSimpleName + " expecting list of expressions")
  }

  override def actionValue(value: OpenlawValue): Result[EthereumSmartContractCall] = VariableType.convert[EthereumSmartContractCall](value)
}
