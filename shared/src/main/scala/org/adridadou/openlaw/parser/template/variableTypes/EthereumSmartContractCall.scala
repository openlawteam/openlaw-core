package org.adridadou.openlaw.parser.template.variableTypes

import java.time.{LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit

import org.adridadou.openlaw.parser.template._
import VariableType._
import org.adridadou.openlaw.parser.template.expressions.Expression
import LocalDateTimeHelper._
import cats.implicits._
import org.adridadou.openlaw.{OpenlawDateTime, OpenlawString, OpenlawValue}

case class EthereumSmartContractCall(
    address: Expression,
    abi: Expression,
    network: Expression,
    functionName: Expression,
    arguments: Seq[Expression],
    startDate: Option[Expression],
    endDate: Option[Expression],
    from: Option[Expression],
    every: Option[Expression]) extends ActionValue with OpenlawValue {
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

  def getInterfaceProtocol(executionResult: TemplateExecutionResult): String =
    getMetadata(abi, executionResult).protocol
  def getInterfaceAddress(executionResult: TemplateExecutionResult): String =
    getMetadata(abi, executionResult).address

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
case class SmartContractCallDetails(
                                     name:VariableName,
                                     call:EthereumSmartContractCall,
                                     executionResult: TemplateExecutionResult,
                                     description:String)
