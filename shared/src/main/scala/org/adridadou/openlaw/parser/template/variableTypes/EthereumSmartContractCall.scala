package org.adridadou.openlaw.parser.template.variableTypes

import java.time.{LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit

import org.adridadou.openlaw.{OpenlawDateTime, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template._
import VariableType._
import org.adridadou.openlaw.parser.template.expressions.Expression
import LocalDateTimeHelper._
import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.oracles.CryptoService

object EthereumSmartContractCall {
  implicit val smartContractEnc: Encoder[EthereumSmartContractCall] = deriveEncoder[EthereumSmartContractCall]
  implicit val smartContractDec: Decoder[EthereumSmartContractCall] = deriveDecoder[EthereumSmartContractCall]
}

case class EthereumSmartContractCall(
    address: Expression,
    abi: Expression,
    network: Expression,
    signatureParameter: Option[Expression],
    signatureRSVParameter: Option[SignatureRSVParameter],
    functionName: Expression,
    arguments: Seq[Expression],
    startDate: Option[Expression],
    endDate: Option[Expression],
    from: Option[Expression],
    every: Option[Expression]) extends ActionValue with OpenlawValue {

  def callKey(executionResult: TemplateExecutionResult, crypto:CryptoService):Option[EthereumData] = for {
    from <- from.flatMap(_.evaluate(executionResult)).map(EthAddressType.convert)
    to <- address.evaluate(executionResult).map(EthAddressType.convert)

  } yield {
    val args = arguments.flatMap(expr => {
      val varType = expr.expressionType(executionResult)
      expr.evaluate(executionResult).map(varType.internalFormat)
    })
    val argHash = EthereumData(crypto.sha256(args.map(arg => EthereumData(crypto.sha256(arg)).withLeading0x).mkString("")))

    EthereumData(crypto.sha256(from.withLeading0x + to.withLeading0x + argHash.withLeading0x))
  }

  def getEvery(executionResult: TemplateExecutionResult): Option[Period] =
    every.map(getPeriod(_ , executionResult))
  def getStartDate(executionResult: TemplateExecutionResult): Option[LocalDateTime] =
    startDate.map(getDate(_, executionResult).localDateTime)
  def getEndDate(executionResult: TemplateExecutionResult): Option[LocalDateTime] =
    endDate.map(getDate(_, executionResult).localDateTime)
  def getFunctionName(executionResult: TemplateExecutionResult): String =
    getString(functionName, executionResult)
  def getContractAddress(executionResult: TemplateExecutionResult): EthereumAddress =
    getEthereumAddress(address, executionResult)
  def getEthereumNetwork(executionResult: TemplateExecutionResult):Option[String] =
    network.evaluate(executionResult)
      .map(VariableType.convert[OpenlawString](_).string)

  def getFrom(executionResult: TemplateExecutionResult):Option[EthereumAddress] = {
    from.flatMap(_.evaluate(executionResult)).map({
      case OpenlawString(strAddr) => EthereumAddress(strAddr)
      case addr:EthereumAddress => addr
    })
  }

  def getInterfaceProtocol(executionResult: TemplateExecutionResult): String =
    getMetadata(abi, executionResult).protocol
  def getInterfaceAddress(executionResult: TemplateExecutionResult): String =
    getMetadata(abi, executionResult).address

  def parameterToIgnore(executionResult: TemplateExecutionResult):Seq[String] = {
    signatureParameter.flatMap(_.evaluate(executionResult)) match {
      case Some(value) =>
        Seq(VariableType.convert[OpenlawString](value).string)
      case None =>
        signatureRSVParameter.flatMap(_.getRsv(executionResult)).map(rsv => Seq(rsv.r, rsv.s, rsv.v)).getOrElse(Seq())
    }
  }

  override def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions:Seq[OpenlawExecution]): Option[LocalDateTime] = {
    val executions = pastExecutions.map(VariableType.convert[EthereumSmartContractExecution])
    val callToReRun = executions
      .map(VariableType.convert[EthereumSmartContractExecution])
      .find(execution => execution.executionStatus match {
        case FailedExecution =>
          execution.executionDate
            .isBefore(LocalDateTime.now(executionResult.clock).minus(5, ChronoUnit.MINUTES))
        case _ =>
          false
      }).map(_.scheduledDate)

    callToReRun orElse {
      executions.map(_.scheduledDate).toList match {
        case Nil =>
          Some(getStartDate(executionResult).getOrElse(LocalDateTime.now(executionResult.clock)))
        case list =>
          val lastDate = list.maxBy(_.toEpochSecond(ZoneOffset.UTC))
          getEvery(executionResult).flatMap(schedulePeriod => {
            DateTimeType
              .plus(Some(lastDate), Some(schedulePeriod), executionResult)
              .map(VariableType.convert[OpenlawDateTime](_).localDateTime)
              .filter(nextDate => getEndDate(executionResult).forall(date => nextDate.isBefore(date) || nextDate === date))
          })
      }
    }
  }
}

case class SmartContractMetadata(protocol: String, address: String) extends OpenlawValue
