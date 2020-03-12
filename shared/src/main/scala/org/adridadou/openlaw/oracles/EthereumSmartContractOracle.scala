package org.adridadou.openlaw.oracles

import java.time.{Instant, LocalDateTime}

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.{ActionIdentifier, ActionInfo}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging
import io.circe.syntax._
import cats.implicits._
import EthereumHash._
import LocalDateTimeHelper._
import org.adridadou.openlaw.result.{Failure, Result, Success}

final case class EthereumSmartContractOracle()
    extends OpenlawOracle[EthereumSmartContractCallEvent]
    with LazyLogging {

  override def incoming(
      vm: OpenlawVm,
      event: EthereumSmartContractCallEvent
  ): Result[OpenlawVm] = event match {
    case failedEvent: FailedEthereumSmartContractCallEvent =>
      handleFailedEvent(vm, failedEvent)
    case _ =>
      handleEvent(vm, event)
  }

  private def handleEvent(
      vm: OpenlawVm,
      event: EthereumSmartContractCallEvent
  ): Result[OpenlawVm] = {
    vm.allActions
      .flatMap { actions =>
        actions
          .map { info =>
            info.identifier.map((info, _))
          }
          .sequence
          .flatMap { list =>
            list
              .find { case (_, id) => id === event.identifier }
              .map {
                case (actionInfo, _) =>
                  vm.executions[EthereumSmartContractExecution](
                      event.identifier
                    )
                    .find(_.tx === event.hash) match {
                    case Some(execution) =>
                      Success(
                        vm.newExecution(
                          event.identifier,
                          createNewExecution(
                            vm,
                            toOpenlawExecutionStatus(event),
                            event,
                            execution.scheduledDate
                          )
                        )
                      )
                    case None =>
                      getScheduledDate(actionInfo, vm, event) flatMap {
                        case Some(scheduleDate) =>
                          Success(
                            vm.newExecution(
                              event.identifier,
                              createNewExecution(
                                vm,
                                toOpenlawExecutionStatus(event),
                                event,
                                scheduleDate
                              )
                            )
                          )
                        case None =>
                          logger.warn(
                            s"the transaction ${event.hash.toString} has not been added yet"
                          )
                          Success(vm)
                      }
                  }
              }
              .getOrElse(
                Failure(s"action not found for event ${event.typeIdentifier}")
              )
          }
      }
  }

  private def handleFailedEvent(
      vm: OpenlawVm,
      event: FailedEthereumSmartContractCallEvent
  ): Result[OpenlawVm] = {
    val failedExecution = EthereumSmartContractExecution(
      scheduledDate = event.scheduledDate,
      executionDate = event.executionDate,
      executionStatus = FailedExecution,
      tx = EthereumHash.empty
    )

    Success(vm.newExecution(event.identifier, failedExecution))
  }

  private def toOpenlawExecutionStatus(
      event: EthereumSmartContractCallEvent
  ): OpenlawExecutionStatus = event match {
    case _: SuccessfulEthereumSmartContractCallEvent =>
      SuccessfulExecution
    case _: PendingEthereumSmartContractCallEvent =>
      PendingExecution
    case _: FailedEthereumSmartContractCallEvent =>
      FailedExecution
  }

  private def createNewExecution(
      vm: OpenlawVm,
      executionStatus: OpenlawExecutionStatus,
      event: EthereumSmartContractCallEvent,
      scheduledDate: Instant
  ): EthereumSmartContractExecution =
    EthereumSmartContractExecution(
      scheduledDate = scheduledDate,
      executionDate = event.executionDate,
      executionStatus = executionStatus,
      tx = event.hash
    )

  private def getScheduledDate(
      info: ActionInfo,
      vm: OpenlawVm,
      event: EthereumSmartContractCallEvent
  ): Result[Option[Instant]] =
    info.identifier.flatMap { id =>
      vm.executions[EthereumSmartContractExecution](id)
        .find(_.tx === event.hash) match {
        case Some(execution) =>
          Success(Some(execution.scheduledDate))
        case None =>
          info.action
            .nextActionSchedule(info.executionResult, vm.executions(id))
      }
    }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _: EthereumSmartContractCallEvent => true
    case _                                 => false
  }
}

object PendingEthereumSmartContractCallEvent {
  implicit val ethereumSmartContractCallEventEnc
      : Encoder[PendingEthereumSmartContractCallEvent] = deriveEncoder
  implicit val ethereumSmartContractCallEventDec
      : Decoder[PendingEthereumSmartContractCallEvent] = deriveDecoder
}

object FailedEthereumSmartContractCallEvent {
  implicit val failedEthereumSmartContractCallEventEnc
      : Encoder[FailedEthereumSmartContractCallEvent] = deriveEncoder
  implicit val failedEthereumSmartContractCallEventDec
      : Decoder[FailedEthereumSmartContractCallEvent] = deriveDecoder
}

object SuccessfulEthereumSmartContractCallEvent {
  implicit val successfulEthereumSmartContractCallEventEnc
      : Encoder[SuccessfulEthereumSmartContractCallEvent] = deriveEncoder
  implicit val successfulEthereumSmartContractCallEventDec
      : Decoder[SuccessfulEthereumSmartContractCallEvent] = deriveDecoder
}

sealed trait EthereumSmartContractCallEvent extends OpenlawVmEvent {
  val identifier: ActionIdentifier
  val hash: EthereumHash
  val executionDate: Instant
}

final case class PendingEthereumSmartContractCallEvent(
    identifier: ActionIdentifier,
    hash: EthereumHash,
    receiveAddress: EthereumAddress,
    executionDate: Instant
) extends EthereumSmartContractCallEvent {
  override def typeIdentifier: String =
    className[PendingEthereumSmartContractCallEvent]
  override def serialize: String = this.asJson.noSpaces
}

final case class SuccessfulEthereumSmartContractCallEvent(
    identifier: ActionIdentifier,
    hash: EthereumHash,
    receiveAddress: EthereumAddress,
    executionDate: Instant
) extends EthereumSmartContractCallEvent {
  override def typeIdentifier: String =
    className[SuccessfulEthereumSmartContractCallEvent]
  override def serialize: String = this.asJson.noSpaces
}

final case class FailedEthereumSmartContractCallEvent(
    identifier: ActionIdentifier,
    hash: EthereumHash,
    errorMessage: String,
    scheduledDate: Instant,
    executionDate: Instant
) extends EthereumSmartContractCallEvent {
  override def typeIdentifier: String =
    className[FailedEthereumSmartContractCallEvent]
  override def serialize: String = this.asJson.noSpaces
}
