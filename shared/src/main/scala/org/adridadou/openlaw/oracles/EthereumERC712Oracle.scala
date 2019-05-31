package org.adridadou.openlaw.oracles

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.VariableName
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent, OpenlawVmInitEvent}
import slogging.LazyLogging
import io.circe.syntax._
import cats.implicits._
import LocalDateTimeHelper._
import cats.kernel.Eq
import org.adridadou.openlaw.result.{Failure, Result, Success}

case class EthereumERC712Oracle(crypto:CryptoService) extends OpenlawOracle[PreparedERC712SmartContractCallEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: PreparedERC712SmartContractCallEvent): Result[OpenlawVm] = {
    vm
      .getAllVariables(EthereumCallType)
      .find({ case (_, variable) => variable.name === event.name }).flatMap { case (executionResult, variable) => variable.evaluateT[EthereumSmartContractCall](executionResult).map((_, variable.name, executionResult)) }
      .flatMap({ case (call, name, executionResult) => for {
        from <- call.getFrom(executionResult) if from === event.signee
        to <- Some(call.getContractAddress(executionResult)) if to === event.receiveAddress
      } yield name})
      .filter(_ === event.name)
      .map { name => Success(vm.setInitExecution(name, PreparedERC712SmartContractCallExecution(name, event.signedCall))) }
      .getOrElse(Failure(s"action not found for ${event.name.name}. available ${vm.allNextActions.map(_.name.name).mkString(",")}"))
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:PreparedERC712SmartContractCallEvent => true
    case _ => false
  }
}

object PreparedERC712SmartContractCallEvent {
  implicit val preparedERC712SmartContractCallEventEnc: Encoder[PreparedERC712SmartContractCallEvent] = deriveEncoder[PreparedERC712SmartContractCallEvent]
  implicit val preparedERC712SmartContractCallEventDec: Decoder[PreparedERC712SmartContractCallEvent] = deriveDecoder[PreparedERC712SmartContractCallEvent]

  implicit val preparedErc712SmartContractCallEventEq:Eq[PreparedERC712SmartContractCallEvent] = Eq.fromUniversalEquals
}

object PreparedERC712SmartContractCallExecution{
  implicit val preparedERC712SmartContractCallExecutionEnc: Encoder[PreparedERC712SmartContractCallExecution] = deriveEncoder[PreparedERC712SmartContractCallExecution]
  implicit val preparedERC712SmartContractCallExecutionDec: Decoder[PreparedERC712SmartContractCallExecution] = deriveDecoder[PreparedERC712SmartContractCallExecution]

  implicit val preparedErc712SmartContractCallExecutionEq:Eq[PreparedERC712SmartContractCallExecution] = Eq.fromUniversalEquals
}

final case class PreparedERC712SmartContractCallEvent(
                                                       name:VariableName,
                                                       signee:EthereumAddress,
                                                       signedCall:EthereumSignature,
                                                       receiveAddress:EthereumAddress) extends OpenlawVmInitEvent {
  override def typeIdentifier: String = className[PreparedERC712SmartContractCallEvent]
  override def serialize: String = this.asJson.noSpaces
}

final case class PreparedERC712SmartContractCallExecution(
                                                           name:VariableName,
                                                           signedCall:EthereumSignature) extends OpenlawExecutionInit {
  override def typeIdentifier: String = className[PreparedERC712SmartContractCallExecution]
  override def serialize: String = this.asJson.noSpaces
}