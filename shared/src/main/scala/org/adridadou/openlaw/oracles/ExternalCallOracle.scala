package org.adridadou.openlaw.oracles

import java.time.Instant

import org.adridadou.openlaw.parser.template.{
  ActionIdentifier,
  ActionInfo,
  TemplateExecutionResult,
  VariableName
}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result._
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import org.adridadou.openlaw.{OpenlawMap, OpenlawValue}
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import cats.implicits._
import cats.kernel.Eq
import LocalDateTimeHelper._

final case class ExternalCallOracle(
    crypto: CryptoService,
    externalSignatureAccounts: Map[ServiceName, EthereumAddress] = Map.empty
) extends OpenlawOracle[ExternalCallEvent] {

  override def incoming(
      vm: OpenlawVm,
      event: ExternalCallEvent
  ): Result[OpenlawVm] = event match {
    case failedEvent: FailedExternalCallEvent =>
      handleFailedEvent(vm, failedEvent, failedEvent.scheduledDate)
    case successEvent: SuccessfulExternalCallEvent =>
      handleSuccessEvent(vm, successEvent)
    case pending: PendingExternalCallEvent if pending.isExternalSignature =>
      Success(
        vm.newExecution(
          event.identifier,
          event.toExecution(event.executionDate)
        )
      )
    case _ =>
      handleEvent(vm, event)
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _: ExternalCallEvent => true
    case _                    => false
  }

  private def handleFailedEvent(
      vm: OpenlawVm,
      event: FailedExternalCallEvent,
      scheduledDate: Instant
  ): Result[OpenlawVm] = {
    val failedExecution = FailedExternalCallExecution(
      scheduledDate = scheduledDate,
      executionDate = event.executionDate,
      errorMessage = event.errorMessage,
      requestIdentifier = event.requestIdentifier
    )

    Success(vm.newExecution(event.identifier, failedExecution))
  }

  private def handleSuccessEvent(
      vm: OpenlawVm,
      event: SuccessfulExternalCallEvent
  ): Result[OpenlawVm] =
    getExternalServiceAccount(event.serviceName).flatMap(account =>
      getSignedData(event.caller, event.identifier, event.result)
        .flatMap(signedData => verify(signedData, account, event.signature))
    ) flatMap (_ => handleEvent(vm, event))

  private def handleEvent(
      vm: OpenlawVm,
      event: ExternalCallEvent
  ): Result[OpenlawVm] =
    vm.allActions.flatMap { seq =>
      seq
        .map(info => info.identifier.map(identifier => (identifier, info)))
        .sequence
        .flatMap { seq =>
          seq
            .find { case (identifier, _) => identifier === event.identifier }
            .map {
              case (_, actionInfo) =>
                vm.executions[ExternalCallExecution](event.identifier)
                  .find(_.requestIdentifier === event.requestIdentifier) match {
                  case Some(execution: PendingExternalCallExecution) =>
                    Success(
                      vm.newExecution(
                        event.identifier,
                        event.toExecution(execution.scheduledDate)
                      )
                    )
                  case _ =>
                    getScheduledDate(actionInfo, vm, event) flatMap {
                      case Some(scheduleDate) =>
                        Success(
                          vm.newExecution(
                            event.identifier,
                            event.toExecution(scheduleDate)
                          )
                        )
                      case None =>
                        Success(vm)
                    }
                }
            }
            .getOrElse(
              Failure(s"action not found for event ${event.typeIdentifier}")
            )
        }
    }

  private def getScheduledDate(
      info: ActionInfo,
      vm: OpenlawVm,
      event: ExternalCallEvent
  ): Result[Option[Instant]] =
    for {
      id <- info.identifier
      scheduledDate <- vm
        .executions[ExternalCallExecution](id)
        .find(_.requestIdentifier === event.requestIdentifier) match {
        case Some(execution: PendingExternalCallExecution) =>
          Success(Some(execution.scheduledDate))

        case _ =>
          info.action
            .nextActionSchedule(info.executionResult, vm.executions(id))
      }
    } yield scheduledDate

  private def getExternalServiceAccount(
      serviceName: ServiceName
  ): Result[EthereumAddress] =
    externalSignatureAccounts.get(serviceName) match {
      case Some(account) => Success(account)
      case None          => Failure(s"unknown service $serviceName")
    }

  private def getSignedData(
      caller: Caller,
      identifier: ActionIdentifier,
      data: String
  ): Result[EthereumData] =
    attempt(
      EthereumData(crypto.sha256(caller.id))
        .merge(EthereumData(crypto.sha256(identifier.identifier)))
        .merge(EthereumData(crypto.sha256(data)))
    ) match {
      case Success(signedData) => Success(signedData)
      case Failure(_, msg)     => Failure(s"Unable to get signed data, error: $msg")
    }

  private def verify(
      signedData: EthereumData,
      account: EthereumAddress,
      signature: EthereumSignature
  ): Result[String] =
    EthereumAddress(
      crypto.validateECSignature(signedData.data, signature.signature)
    ).map { derivedAddress =>
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

object Caller {
  implicit val callerEnc: Encoder[Caller] = deriveEncoder
  implicit val callerDec: Decoder[Caller] = deriveDecoder
  implicit val callerEq: Eq[Caller] = Eq.fromUniversalEquals
}

object PendingExternalCallEvent {
  implicit val pendingExternalCallEventEnc: Encoder[PendingExternalCallEvent] =
    deriveEncoder
  implicit val pendingExternalCallEventDec: Decoder[PendingExternalCallEvent] =
    deriveDecoder
  implicit val pendingExternalCallEventEq: Eq[PendingExternalCallEvent] =
    Eq.fromUniversalEquals
}

object FailedExternalCallEvent {
  implicit val failedExternalCallEventEnc: Encoder[FailedExternalCallEvent] =
    deriveEncoder
  implicit val failedExternalCallEventDec: Decoder[FailedExternalCallEvent] =
    deriveDecoder
  implicit val failedExternalCallEventEq: Eq[FailedExternalCallEvent] =
    Eq.fromUniversalEquals
}

object SuccessfulExternalCallEvent {
  implicit val successfulExternalCallEventEnc
      : Encoder[SuccessfulExternalCallEvent] = deriveEncoder
  implicit val successfulExternalCallEventDec
      : Decoder[SuccessfulExternalCallEvent] = deriveDecoder
  implicit val successfulExternalCallEventEq: Eq[SuccessfulExternalCallEvent] =
    Eq.fromUniversalEquals
}

final case class Caller(id: String)

sealed trait ExternalCallEvent extends OpenlawVmEvent {
  val caller: Caller
  val identifier: ActionIdentifier
  val requestIdentifier: RequestIdentifier
  val executionDate: Instant

  def toExecution(scheduledDate: Instant): ExternalCallExecution
}

final case class PendingExternalCallEvent(
    caller: Caller,
    identifier: ActionIdentifier,
    requestIdentifier: RequestIdentifier,
    executionDate: Instant
) extends ExternalCallEvent {

  def isExternalSignature: Boolean =
    identifier.identifier.startsWith(SignatureAction.bulkRequestIdentifier)

  override def typeIdentifier: String = className[PendingExternalCallEvent]

  override def serialize: String = this.asJson.noSpaces

  override def toExecution(
      scheduledDate: Instant
  ): ExternalCallExecution = PendingExternalCallExecution(
    scheduledDate = scheduledDate,
    executionDate = executionDate,
    requestIdentifier = requestIdentifier,
    actionIdentifier = Some(identifier)
  )
}

final case class FailedExternalCallEvent(
    caller: Caller,
    identifier: ActionIdentifier,
    requestIdentifier: RequestIdentifier,
    scheduledDate: Instant,
    executionDate: Instant,
    errorMessage: String
) extends ExternalCallEvent {
  override def typeIdentifier: String =
    className[FailedEthereumSmartContractCallEvent]

  override def serialize: String = this.asJson.noSpaces

  override def toExecution(
      scheduledDate: Instant
  ): ExternalCallExecution = FailedExternalCallExecution(
    scheduledDate = scheduledDate,
    executionDate = executionDate,
    errorMessage = errorMessage,
    requestIdentifier = requestIdentifier
  )
}

final case class SuccessfulExternalCallEvent(
    caller: Caller,
    identifier: ActionIdentifier,
    requestIdentifier: RequestIdentifier,
    executionDate: Instant,
    result: String,
    serviceName: ServiceName,
    signature: EthereumSignature
) extends ExternalCallEvent {

  override def typeIdentifier: String = className[SuccessfulExternalCallEvent]

  override def serialize: String = this.asJson.noSpaces

  def results(
      structure: DefinedStructureType,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawMap[VariableName, OpenlawValue]] =
    structure.cast(result, executionResult)

  override def toExecution(
      scheduledDate: Instant
  ): ExternalCallExecution = SuccessfulExternalCallExecution(
    scheduledDate = scheduledDate,
    executionDate = executionDate,
    result = result,
    requestIdentifier = requestIdentifier
  )

}
