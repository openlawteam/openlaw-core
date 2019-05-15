package org.adridadou.openlaw.oracles

import java.time.LocalDateTime

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.{ActionIdentifier, ActionInfo, TemplateExecutionResult, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging
import io.circe.syntax._
import cats.implicits._
import LocalDateTimeHelper._
import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.result.{Failure, Result, Success}


case class ExternalCallOracle(crypto: CryptoService) extends OpenlawOracle[ExternalCallEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: ExternalCallEvent): Result[OpenlawVm] = event match {
    case failedEvent: FailedExternalCallEvent =>
      handleFailedEvent(vm, failedEvent)
    case _ =>
      handleEvent(vm, event)
  }

  private def handleEvent(vm: OpenlawVm, event: ExternalCallEvent): Result[OpenlawVm] = {
    //TODO check signatures
    vm.allActions.find(info => info.identifier === event.identifier).map { actionInfo =>
        vm.executions[ExternalCallExecution](event.identifier).find(_.requestIdentifier === event.requestIdentifier) match {
          case Some(execution) =>
            Success(vm.newExecution(event.identifier, event.toExecution(execution.scheduledDate)))
          case None =>
            getScheduledDate(actionInfo, vm, event) match {
              case Some(scheduleDate) =>
                Success(vm.newExecution(event.identifier, event.toExecution(scheduleDate)))
              case None =>
                logger.warn(s"the external call ${event.requestIdentifier} has not been scheduled yet")
                Success(vm)
            }
        }
      }.getOrElse(Failure(s"action not found for event ${event.typeIdentifier}"))
  }

  private def handleFailedEvent(vm: OpenlawVm, event: FailedExternalCallEvent): Result[OpenlawVm] = {
    val failedExecution = FailedExternalCallExecution(
      scheduledDate = event.scheduledDate,
      executionDate = event.executionDate,
      errorMessage = event.errorMessage,
      requestIdentifier = event.requestIdentifier)

    Success(vm.newExecution(event.identifier, failedExecution))
  }

  private def getScheduledDate(info: ActionInfo, vm: OpenlawVm, event: ExternalCallEvent): Option[LocalDateTime] = {
    vm.executions[ExternalCallExecution](info.identifier).find(_.requestIdentifier === event.requestIdentifier) match {
      case Some(execution) =>
        Some(execution.scheduledDate)
      case None =>
        info.action.nextActionSchedule(info.executionResult, vm.executions(event.identifier))
    }
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _: ExternalCallEvent => true
    case _ => false
  }
}

object PendingExternalCallEvent {
  implicit val pendingExternalCallEventEnc: Encoder[PendingExternalCallEvent] = deriveEncoder[PendingExternalCallEvent]
  implicit val pendingExternalCallEventDec: Decoder[PendingExternalCallEvent] = deriveDecoder[PendingExternalCallEvent]
}

object FailedExternalCallEvent {
  implicit val failedExternalCallEventEnc: Encoder[FailedExternalCallEvent] = deriveEncoder[FailedExternalCallEvent]
  implicit val failedExternalCallEventDec: Decoder[FailedExternalCallEvent] = deriveDecoder[FailedExternalCallEvent]
}

object SuccessfulExternalCallEvent {
  implicit val successfulExternalCallEventEnc: Encoder[SuccessfulExternalCallEvent] = deriveEncoder[SuccessfulExternalCallEvent]
  implicit val successfulExternalCallEventDec: Decoder[SuccessfulExternalCallEvent] = deriveDecoder[SuccessfulExternalCallEvent]
}

sealed trait ExternalCallEvent extends OpenlawVmEvent {
  val identifier:ActionIdentifier
  val requestIdentifier: RequestIdentifier
  val executionDate: LocalDateTime
  def toExecution(scheduledDate:LocalDateTime):ExternalCallExecution
}

final case class PendingExternalCallEvent(identifier: ActionIdentifier,
                                          requestIdentifier: RequestIdentifier,
                                          executionDate: LocalDateTime) extends ExternalCallEvent {
  override def typeIdentifier: String = className[PendingExternalCallEvent]

  override def serialize: String = this.asJson.noSpaces

  override def toExecution(scheduledDate: LocalDateTime): ExternalCallExecution = PendingExternalCallExecution(
    scheduledDate = scheduledDate,
    executionDate = executionDate,
    requestIdentifier = requestIdentifier
  )
}

final case class FailedExternalCallEvent(identifier: ActionIdentifier,
                                         requestIdentifier: RequestIdentifier,
                                         executionDate: LocalDateTime,
                                         scheduledDate: LocalDateTime,
                                         errorMessage: String) extends ExternalCallEvent {
  override def typeIdentifier: String = className[FailedEthereumSmartContractCallEvent]

  override def serialize: String = this.asJson.noSpaces

  override def toExecution(scheduledDate: LocalDateTime): ExternalCallExecution = FailedExternalCallExecution(
    scheduledDate = scheduledDate,
    executionDate = executionDate,
    errorMessage = errorMessage,
    requestIdentifier = requestIdentifier
  )
}


final case class SuccessfulExternalCallEvent(identifier: ActionIdentifier,
                                             requestIdentifier: RequestIdentifier,
                                             executionDate: LocalDateTime,
                                             result: String) extends ExternalCallEvent {
  override def typeIdentifier: String = className[SuccessfulExternalCallEvent]

  override def serialize: String = this.asJson.noSpaces

  def results(structure:DefinedStructureType, executionResult:TemplateExecutionResult): Map[VariableName, OpenlawValue] =
    structure.cast(result, executionResult).underlying

  override def toExecution(scheduledDate:LocalDateTime): ExternalCallExecution = SuccessfulExternalCallExecution(
    scheduledDate = scheduledDate,
    executionDate = executionDate,
    result = result,
    requestIdentifier = requestIdentifier
  )
}