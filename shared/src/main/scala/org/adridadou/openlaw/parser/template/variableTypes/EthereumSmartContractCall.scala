package org.adridadou.openlaw.parser.template.variableTypes

import java.time.{LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit

import org.adridadou.openlaw.{OpenlawDateTime, OpenlawNativeValue, OpenlawString}
import org.adridadou.openlaw.parser.template._
import VariableType._
import org.adridadou.openlaw.parser.template.expressions.Expression
import LocalDateTimeHelper._
import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.oracles.CryptoService
import org.adridadou.openlaw.result.{Result, Success}

object EthereumSmartContractCall {
  implicit val smartContractEnc: Encoder[EthereumSmartContractCall] = deriveEncoder
  implicit val smartContractDec: Decoder[EthereumSmartContractCall] = deriveDecoder
}

final case class EthereumSmartContractCall(
    address: Expression,
    abi: Expression,
    network: Expression,
    signatureParameter: Option[Expression],
    signatureRSVParameter: Option[SignatureRSVParameter],
		value: Option[Expression],
    functionName: Expression,
    arguments: Seq[Expression],
    startDate: Option[Expression],
    endDate: Option[Expression],
    from: Option[Expression],
    every: Option[Expression]) extends ActionValue with OpenlawNativeValue {

  def isERC712Call:Boolean = signatureParameter.isDefined || signatureRSVParameter.isDefined

  def callKey(executionResult: TemplateExecutionResult, crypto:CryptoService): Result[Option[EthereumData]] = {
    (for {
      fromResult <- from.flatMap(_.evaluate(executionResult).flatMap(_.map(EthAddressType.convert).sequence).sequence)
      toResult <- address.evaluate(executionResult).flatMap(_.map(EthAddressType.convert).sequence).sequence
    } yield {
      for {
        from <- fromResult
        to <- toResult
        ethereumData <- {
          val args = arguments.toList.map { expr =>
            val x = expr.expressionType(executionResult).flatMap(varType => expr.evaluate(executionResult).flatMap(_.map(varType.internalFormat).sequence))
            x
          }
          .sequence
          .map(_.flatten)
          .map { args =>
            val argHash = EthereumData(crypto.sha256(args.map(arg => EthereumData(crypto.sha256(arg)).withLeading0x).mkString("")))
            EthereumData(crypto.sha256(from.withLeading0x + to.withLeading0x + argHash.withLeading0x))
          }
          args
        }
      } yield ethereumData
    }).sequence
  }

  def getEvery(executionResult: TemplateExecutionResult): Result[Option[Period]] =
    every.map(getPeriod(_ , executionResult)).sequence
  def getStartDate(executionResult: TemplateExecutionResult): Result[Option[LocalDateTime]] =
    startDate.map(getDate(_, executionResult).map(_.underlying)).sequence
  def getEndDate(executionResult: TemplateExecutionResult): Result[Option[LocalDateTime]] =
    endDate.map(getDate(_, executionResult).map(_.underlying)).sequence
  def getFunctionName(executionResult: TemplateExecutionResult): Result[String] =
    getString(functionName, executionResult)
  def getContractAddress(executionResult: TemplateExecutionResult): Result[EthereumAddress] =
    getEthereumAddress(address, executionResult)
  def getEthereumNetwork(executionResult: TemplateExecutionResult): Result[Option[String]] =
    network.evaluate(executionResult).flatMap(_.map(VariableType.convert[OpenlawString]).sequence)

  def getFrom(executionResult: TemplateExecutionResult):Result[Option[EthereumAddress]] =
    from
      .flatMap(_.evaluate(executionResult).sequence)
      .map {
        _.flatMap {
          case OpenlawString(strAddr) => EthereumAddress(strAddr)
          case addr: EthereumAddress => Success(addr)
        }
      }
      .sequence

  def getInterfaceProtocol(executionResult: TemplateExecutionResult): Result[String] =
    getMetadata(abi, executionResult).map(_.protocol)
  def getInterfaceAddress(executionResult: TemplateExecutionResult): Result[String] =
    getMetadata(abi, executionResult).map(_.address)

  def parameterToIgnore(executionResult: TemplateExecutionResult): Result[List[String]] =
    (for {
      signatureParameterValue <- signatureParameter
      signatureRSVParameterValue <- signatureRSVParameter
      result <- {
        signatureParameterValue
          .evaluate(executionResult)
          .flatMap {
            case Some(value) =>
              VariableType.convert[OpenlawString](value).map(x => Some(List(x)))
            case None =>
              signatureRSVParameterValue.getRsv(executionResult).map(_.map(rsv => List(rsv.r, rsv.s, rsv.v)))
          }
          .sequence
      }
    } yield result).getOrElse(Success(Nil))

  override def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions:List[OpenlawExecution]): Result[Option[LocalDateTime]] = for {
      executions <- pastExecutions.map(VariableType.convert[EthereumSmartContractExecution]).sequence
      result <- {
        val callToRerun: Option[LocalDateTime] = executions
          .find { execution =>
            execution.executionStatus match {
              case FailedExecution =>
                execution.executionDate
                  .isBefore(LocalDateTime.now(executionResult.clock).minus(5, ChronoUnit.MINUTES))
              case _ =>
                false
            }
          }
          .map(_.scheduledDate)

        callToRerun.map(Success(_)).orElse {
          executions.map(_.scheduledDate) match {
            case Nil =>
              Some(getStartDate(executionResult).map(_.getOrElse(LocalDateTime.now(executionResult.clock))))
            case list =>
              val lastDate = list.maxBy(_.toEpochSecond(ZoneOffset.UTC))
              (for {
                schedulePeriodOption <- getEvery(executionResult)
                endDate <- getEndDate(executionResult)
              } yield {
                schedulePeriodOption.map { schedulePeriod =>
                  DateTimeType
                    .plus(Some(OpenlawDateTime(lastDate)), Some(schedulePeriod), executionResult)
                    .flatMap { p =>
                      p
                        .map(VariableType.convert[OpenlawDateTime](_).map(_.underlying))
                        .sequence
                        .map(pOption => pOption.filter(nextDate => endDate.forall(date => nextDate.isBefore(date) || nextDate === date)))
                    }
                }
                .flatMap(_.sequence)
              }).sequence.map(_.flatten)
        }
      }.sequence
     }
   } yield result

  override def identifier(executionResult: TemplateExecutionResult): Result[ActionIdentifier] =
    for {
      address <- address.evaluate(executionResult).flatMap(_.map(EthAddressType.convert).sequence)
      arguments <- arguments.toList.map(_.evaluate(executionResult)).sequence.map(_.mkString(","))
      from <- from.map(_.evaluate(executionResult).flatMap(_.map(EthAddressType.convert).sequence)).sequence.map(_.flatten)
      abi <- abi.evaluateT[OpenlawString](executionResult)
      network <- network.evaluateT[OpenlawString](executionResult)
      functionName <- functionName.evaluateT[OpenlawString](executionResult)
    } yield {
      ActionIdentifier(
        address.map(_.withLeading0x).getOrElse("") +
        abi.getOrElse("") +
        network.getOrElse("") +
        functionName.getOrElse("") +
        arguments +
        from.map(_.withLeading0x).getOrElse("")
      )
    }
}

final case class SmartContractMetadata(protocol: String, address: String) extends OpenlawNativeValue
