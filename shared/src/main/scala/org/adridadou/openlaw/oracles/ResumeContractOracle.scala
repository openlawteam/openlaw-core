package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm._
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.result.Result

final case class ResumeContractOracle(crypto:CryptoService) extends SignedActionOracle[ResumeExecutionEvent] {

  override def incoming(vm:OpenlawVm, event: ResumeExecutionEvent): Result[OpenlawVm] =
    checkAction(vm, crypto, event, vm.contractDefinition.id(crypto).resumeContract(crypto))

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:ResumeExecutionEvent => true
    case _ => false
  }

  def processEvent(vm:OpenlawVm, event:ResumeExecutionEvent, name: String, identity:Identity): Result[OpenlawVm] = {
    vm(UpdateExecutionStateCommand(ContractResumed, event))
  }
}

object ResumeExecutionEvent {
  implicit val resumeExecutionEventEnc:Encoder[ResumeExecutionEvent] = deriveEncoder
  implicit val resumeExecutionEventDec:Decoder[ResumeExecutionEvent] = deriveDecoder
}

final case class ResumeExecutionEvent(signature: EthereumSignature) extends SignedActionEvent {
  override def typeIdentifier: String = className[ResumeExecutionEvent]
  override def serialize: String = this.asJson.noSpaces
}

