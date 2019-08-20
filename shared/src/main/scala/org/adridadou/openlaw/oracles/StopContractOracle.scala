package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm._
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.result.Result

final case class StopContractOracle(crypto:CryptoService) extends SignedActionOracle[StopExecutionEvent] {

  override def incoming(vm:OpenlawVm, event: StopExecutionEvent): Result[OpenlawVm] =
    checkAction(vm, crypto, event, vm.contractDefinition.id(crypto).stopContract(crypto))

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:StopExecutionEvent => true
    case _ => false
  }

  def processEvent(vm:OpenlawVm, event:StopExecutionEvent, name: String, identity:Identity): Result[OpenlawVm] = {
    vm(UpdateExecutionStateCommand(ContractStopped, event))
  }
}

object StopExecutionEvent {
  implicit val stopExecutionEventEnc:Encoder[StopExecutionEvent] = deriveEncoder
  implicit val stopExecutionEventDec:Decoder[StopExecutionEvent] = deriveDecoder
}

final case class StopExecutionEvent(signature: EthereumSignature) extends SignedActionEvent {
  override def typeIdentifier: String = className[StopExecutionEvent]
  override def serialize: String = this.asJson.noSpaces
}

