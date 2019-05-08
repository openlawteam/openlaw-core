package org.adridadou.openlaw.oracles

import java.time.LocalDateTime

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.{ActionInfo, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging
import io.circe.syntax._
import cats.implicits._
import LocalDateTimeHelper._
import org.adridadou.openlaw.result.{Failure, Result, Success}

case class EthereumERC712Oracle() extends OpenlawOracle[PreparedERC712SmartContractCallEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: PreparedERC712SmartContractCallEvent): Result[OpenlawVm] = {
    vm.allActions.find(info => info.name === event.name).flatMap {
      case ActionInfo(_:EthereumSmartContractCall, name, _) =>
        Some(Success(vm.initExecution(name, PreparedERC712SmartContractCallExecution(name, event.signedCall))))
      case _ => None
    }.getOrElse(Failure(s"action not found for ${event.name.name}. available ${vm.allNextActions.map(_.name.name).mkString(",")}"))
  }

  private def createNewExecution(vm:OpenlawVm, executionStatus:OpenlawExecutionStatus, event:EthereumSmartContractCallEvent, scheduledDate:LocalDateTime):EthereumSmartContractExecution = EthereumSmartContractExecution(
    scheduledDate = scheduledDate,
    executionDate = event.executionDate,
    executionStatus = executionStatus,
    tx = event.hash)

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:PreparedERC712SmartContractCallEvent => true
    case _ => false
  }
}

object PreparedERC712SmartContractCallEvent {
  implicit val preparedERC712SmartContractCallEventEnc: Encoder[PreparedERC712SmartContractCallEvent] = deriveEncoder[PreparedERC712SmartContractCallEvent]
  implicit val preparedERC712SmartContractCallEventDec: Decoder[PreparedERC712SmartContractCallEvent] = deriveDecoder[PreparedERC712SmartContractCallEvent]
}

object PreparedERC712SmartContractCallExecution{
  implicit val preparedERC712SmartContractCallExecutionEnc: Encoder[PreparedERC712SmartContractCallExecution] = deriveEncoder[PreparedERC712SmartContractCallExecution]
  implicit val preparedERC712SmartContractCallExecutionDec: Decoder[PreparedERC712SmartContractCallExecution] = deriveDecoder[PreparedERC712SmartContractCallExecution]
}

final case class PreparedERC712SmartContractCallEvent(
                                                       name:VariableName,
                                                       signedCall:EthereumData,
                                                       receiveAddress:EthereumAddress) extends OpenlawVmEvent {
  override def typeIdentifier: String = className[PreparedERC712SmartContractCallEvent]
  override def serialize: String = this.asJson.noSpaces
}

final case class PreparedERC712SmartContractCallExecution(
                                                           name:VariableName,
                                                           signedCall:EthereumData) extends OpenlawExecutionInit {
  override def typeIdentifier: String = className[PreparedERC712SmartContractCallExecution]
  override def serialize: String = this.asJson.noSpaces
}