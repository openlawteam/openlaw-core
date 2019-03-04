package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import java.time.{LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit

import org.adridadou.openlaw.parser.template._
import VariableType._
import org.adridadou.openlaw.parser.template.expressions.Expression
import LocalDateTimeHelper._
import cats.implicits._
import org.adridadou.openlaw.result.{Result, Success}

case class EthereumSmartContractCall(
  address: Expression,
  metadata: Expression,
  network: Expression,
  functionName: Expression,
  arguments: Seq[Expression],
  startDate: Option[Expression],
  endDate: Option[Expression],
  every: Option[Expression]) extends ActionValue {
  def getEvery(executionResult: TemplateExecutionResult): Option[Result[Period]] =
    every.map(getPeriod(_ , executionResult))
  def getStartDate(executionResult: TemplateExecutionResult): Option[Result[LocalDateTime]] =
    startDate.map(getDate(_, executionResult))
  def getEndDate(executionResult: TemplateExecutionResult): Option[Result[LocalDateTime]] =
    endDate.map(getDate(_, executionResult))
  def getFunctionName(executionResult: TemplateExecutionResult): Result[String] =
    getString(functionName, executionResult)
  def getContractAddress(executionResult: TemplateExecutionResult): Result[EthereumAddress] =
    getEthereumAddress(address, executionResult)
  def getEthereumNetwork(executionResult: TemplateExecutionResult):Option[Result[String]] =
    network.evaluate(executionResult)
      .map(VariableType.convert[String])

  def getInterfaceProtocol(executionResult: TemplateExecutionResult): Result[String] =
    getMetadata(metadata, executionResult).map(_.protocol)
  def getInterfaceAddress(executionResult: TemplateExecutionResult): Result[String] =
    getMetadata(metadata, executionResult).map(_.address)

  override def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions: Seq[OpenlawExecution]): Result[Option[LocalDateTime]] = {
    pastExecutions.toList.map(VariableType.convert[EthereumSmartContractExecution]).sequence.flatMap { executions =>
      val callToReRun =
        executions
          //.map(VariableType.convert[EthereumSmartContractExecution])  // this appears to be a no-op?
          .find(execution => execution.executionStatus match {
            case FailedExecution =>
              execution.executionDate
                .isBefore(LocalDateTime.now(executionResult.clock).minus(5, ChronoUnit.MINUTES))
            case _ =>
              false
          }).map(_.scheduledDate)

      val y: Result[Option[LocalDateTime]] =
        callToReRun
          .map { localDateTime =>
            val x: Result[Option[LocalDateTime]] = Success(Some(localDateTime))
            x
          }
          .getOrElse {
            executions.map(_.scheduledDate) match {
              case Nil =>
                val x: Result[Option[LocalDateTime]] =
                  getStartDate(executionResult)
                    .sequence
                    .map(option => option.getOrElse(LocalDateTime.now(executionResult.clock)))
                x
              case list =>
                val lastDate = list.maxBy(_.toEpochSecond(ZoneOffset.UTC))
                val b: Result[Option[LocalDateTime]] = getEvery(executionResult).map { schedulePeriod =>
                  val x: Option[Result[LocalDateTime]] = DateTimeType.plus(Some(lastDate), Some(schedulePeriod), executionResult)
                  val z: Result[Option[LocalDateTime]] = x.map(_.flatMap(VariableType.convert[LocalDateTime])).sequence
                  val a: Result[Option[LocalDateTime]] = z.map(_.filter(nextDate => getEndDate(executionResult).forall(date => nextDate.isBefore(date) || nextDate === date)))
                  a.sequence
                }.flatten.sequence
                b
          }
        }

      y
    }
  }
}

case class SmartContractMetadata(protocol: String, address: String)
case class SmartContractCallDetails(
                                     name:VariableName,
                                     call:EthereumSmartContractCall,
                                     executionResult: TemplateExecutionResult,
                                     description:String)
