package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm._
import io.circe.{Decoder, Encoder}
import org.adridadou.openlaw.parser.template.VariableName
import io.circe.syntax._
import io.circe.generic.semiauto._

case class ResumeContractOracle(crypto:CryptoService) extends OpenlawOracle[ResumeExecutionEvent] {

  override def incoming(vm:OpenlawVm, event: ResumeExecutionEvent): Either[String, OpenlawVm] = {
    vm.getAllVariables(IdentityType)
      .map({case (id,variable) => (variable.name, vm.evaluate[Identity](id, variable.name))})
      .find({
        case (_, Right(identity)) =>
          vm.isSignatureValid(vm.contractDefinition.id(crypto).resumeContract(crypto), identity, OpenlawSignatureEvent(vm.contractId, identity.userId, identity.email, event.fullName, event.address, event.signature, EthereumHash.empty))
        case _ => false
      }) match {
      case Some((name:VariableName, Right(identity:Identity))) => processEvent(vm, event, name.name, identity)
      case _ =>
        Left("invalid event! no matching identity while resuming the contract")
    }
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:ResumeExecutionEvent => true
    case _ => false
  }

  private def processEvent(vm:OpenlawVm, event:ResumeExecutionEvent, name: String, identity:Identity):Either[String, OpenlawVm] = {
    vm(UpdateExecutionStateCommand(ContractResumed, event))
  }
}

object ResumeExecutionEvent {
  implicit val resumeExecutionEventEnc:Encoder[ResumeExecutionEvent] = deriveEncoder[ResumeExecutionEvent]
  implicit val resumeExecutionEventDec:Decoder[ResumeExecutionEvent] = deriveDecoder[ResumeExecutionEvent]
}

case class ResumeExecutionEvent(userId: UserId, fullName:String, address: EthereumAddress, signature: EthereumSignature) extends OpenlawVmEvent {
  override def typeIdentifier: String = className[ResumeExecutionEvent]
  override def serialize: String = this.asJson.noSpaces
}

