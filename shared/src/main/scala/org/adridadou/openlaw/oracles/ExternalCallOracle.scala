package org.adridadou.openlaw.oracles

import java.time.LocalDateTime

import org.adridadou.openlaw.parser.template.{ActionIdentifier, ActionInfo, TemplateExecutionResult, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result._
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import org.adridadou.openlaw.{OpenlawMap, OpenlawValue}
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import cats.implicits._
import cats.Eq
import LocalDateTimeHelper._
import org.adridadou.openlaw.values.ContractId
import slogging.LazyLogging

final case class ExternalCallOracle(crypto: CryptoService, externalSignatureAccounts: Map[ServiceName, EthereumAddress] = Map()) extends OpenlawOracle[ExternalCallEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: ExternalCallEvent): Result[OpenlawVm] = event match {
    case failedEvent: FailedExternalCallEvent =>
      handleFailedEvent(vm, failedEvent)
    case successEvent: SuccessfulExternalCallEvent =>
      handleSuccessEvent(vm, successEvent)
    case _ =>
      handleEvent(vm, event)
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _: ExternalCallEvent => true
    case _ => false
  }

  private def handleFailedEvent(vm: OpenlawVm, event: FailedExternalCallEvent): Result[OpenlawVm] = {
    val failedExecution = FailedExternalCallExecution(
      scheduledDate = event.scheduledDate,
      executionDate = event.executionDate,
      errorMessage = event.errorMessage,
      requestIdentifier = event.requestIdentifier)

    Success(vm.newExecution(event.identifier, failedExecution))
  }

  private def handleSuccessEvent(vm: OpenlawVm, event: SuccessfulExternalCallEvent): Result[OpenlawVm] = {
    getExternalServiceAccount(event.serviceName).flatMap(account =>
      getSignedData(event.contractId, event.identifier, event.result).flatMap(signedData =>
        verify(signedData, account, event.signature))) match {
      case Success(_) =>
        handleEvent(vm, event)
      case Failure(_, msg) =>
        handleFailedEvent(vm, FailedExternalCallEvent(
          event.contractId,
          event.identifier,
          event.requestIdentifier,
          event.executionDate,
          event.executionDate,
          msg))
    }
  }

  private def handleEvent(vm: OpenlawVm, event: ExternalCallEvent): Result[OpenlawVm] = {
    vm.allActions.flatMap { seq =>
      seq
        .toList
        .map(info => info.identifier.map(identifier => (identifier, info)))
        .sequence
        .flatMap { seq =>
          seq
            .find { case (identifier, _) => identifier === event.identifier }
            .map { case (_, actionInfo) =>
              vm.executions[ExternalCallExecution](event.identifier).find(_.requestIdentifier === event.requestIdentifier) match {
                case Some(execution) =>
                  Success(vm.newExecution(event.identifier, event.toExecution(execution.scheduledDate)))
                case None =>
                  getScheduledDate(actionInfo, vm, event) flatMap {
                    case Some(scheduleDate) =>
                      Success(vm.newExecution(event.identifier, event.toExecution(scheduleDate)))
                    case None =>
                      logger.warn(s"the external call ${event.requestIdentifier} has not been scheduled yet")
                      Success(vm)
                  }
              }
            }.getOrElse(Failure(s"action not found for event ${event.typeIdentifier}"))
        }
    }
  }

  private def getScheduledDate(info: ActionInfo, vm: OpenlawVm, event: ExternalCallEvent): Result[Option[LocalDateTime]] = {
    info.identifier.flatMap {
      id =>
        vm.executions[ExternalCallExecution](id).find(_.requestIdentifier === event.requestIdentifier) match {
          case Some(execution) =>
            Success(Some(execution.scheduledDate))
          case None =>
            info.action.nextActionSchedule(info.executionResult, vm.executions(id))
        }
    }
  }

  private def getExternalServiceAccount(serviceName: ServiceName): Result[EthereumAddress] =
    externalSignatureAccounts.get(serviceName) match {
      case Some(account) => Success(account)
      case None => Failure(s"unknown service $serviceName")
    }

  private def getSignedData(contractId: ContractId, identifier: ActionIdentifier, data: String): Result[EthereumData] =
    attempt(EthereumData(crypto.sha256(contractId.id))
      .merge(EthereumData(crypto.sha256(identifier.identifier)))
      .merge(EthereumData(crypto.sha256(data)))) match {
      case Success(account) => Success(account)
      case Failure(_, msg) => Failure(s"Unable to get signed data, error: $msg")
    }

  private def verify(signedData: EthereumData, account: EthereumAddress, signature: EthereumSignature): Result[String] =
    EthereumAddress(crypto.validateECSignature(signedData.data, signature.signature))
      .map { derivedAddress =>
        account.withLeading0x === derivedAddress.withLeading0x
      }
      .flatMap { isValid =>
        if (isValid) {
          Success("Valid signature for result data")
        } else {
          Failure("Invalid signature for result data")
        }
      }

}

object PendingExternalCallEvent {
  implicit val pendingExternalCallEventEnc: Encoder[PendingExternalCallEvent] = deriveEncoder
  implicit val pendingExternalCallEventDec: Decoder[PendingExternalCallEvent] = deriveDecoder
}

object FailedExternalCallEvent {
  implicit val failedExternalCallEventEnc: Encoder[FailedExternalCallEvent] = deriveEncoder
  implicit val failedExternalCallEventDec: Decoder[FailedExternalCallEvent] = deriveDecoder
}

object SuccessfulExternalCallEvent {
  implicit val successfulExternalCallEventEnc: Encoder[SuccessfulExternalCallEvent] = deriveEncoder
  implicit val successfulExternalCallEventDec: Decoder[SuccessfulExternalCallEvent] = deriveDecoder
}

sealed trait ExternalCallEvent extends OpenlawVmEvent {
  val contractId: ContractId
  val identifier: ActionIdentifier
  val requestIdentifier: RequestIdentifier
  val executionDate: LocalDateTime

  def toExecution(scheduledDate: LocalDateTime): ExternalCallExecution
}

final case class PendingExternalCallEvent(contractId: ContractId,
                                          identifier: ActionIdentifier,
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

final case class FailedExternalCallEvent(contractId: ContractId,
                                         identifier: ActionIdentifier,
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


final case class SuccessfulExternalCallEvent(contractId: ContractId,
                                             identifier: ActionIdentifier,
                                             requestIdentifier: RequestIdentifier,
                                             executionDate: LocalDateTime,
                                             result: String,
                                             serviceName: ServiceName,
                                             signature: EthereumSignature) extends ExternalCallEvent {

  override def typeIdentifier: String = className[SuccessfulExternalCallEvent]

  override def serialize: String = this.asJson.noSpaces

  def results(structure: DefinedStructureType, executionResult: TemplateExecutionResult): Result[OpenlawMap[VariableName, OpenlawValue]] =
    structure.cast(result, executionResult)

  override def toExecution(scheduledDate: LocalDateTime): ExternalCallExecution = SuccessfulExternalCallExecution(
    scheduledDate = scheduledDate,
    executionDate = executionDate,
    result = result,
    requestIdentifier = requestIdentifier
  )

}
