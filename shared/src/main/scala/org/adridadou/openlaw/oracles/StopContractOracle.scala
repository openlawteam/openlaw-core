package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm._
import io.circe.{Decoder, Encoder}
import org.adridadou.openlaw.parser.template.VariableName
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.IdentityOpenlawValue
import org.adridadou.openlaw.result.{Failure, Result}

case class StopContractOracle(crypto:CryptoService) extends OpenlawOracle[StopExecutionEvent] {

  override def incoming(vm:OpenlawVm, event: StopExecutionEvent): Result[OpenlawVm] = {
    vm.getAllVariables(IdentityType)
      .map({case (id,variable) => (variable.name, vm.evaluate[IdentityOpenlawValue](id, variable.name))})
      .find({
        case (_, Right(identity)) =>
          vm.isSignatureValid(vm.contractDefinition.id(crypto).stopContract(crypto), OpenlawSignatureEvent(vm.contractId, identity.email, "", event.signature, EthereumHash.empty))
        case _ => false
      }) match {
      case Some((name:VariableName, Right(identity:Identity))) => processEvent(vm, event, name.name, identity)
      case _ =>
        Failure("invalid event! no matching identity while stopping the contract")
    }
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:StopExecutionEvent => true
    case _ => false
  }

  private def processEvent(vm:OpenlawVm, event:StopExecutionEvent, name: String, identity:Identity): Result[OpenlawVm] = {
    vm(UpdateExecutionStateCommand(ContractStopped, event))
  }
}

object StopExecutionEvent {
  implicit val stopExecutionEventEnc:Encoder[StopExecutionEvent] = deriveEncoder[StopExecutionEvent]
  implicit val stopExecutionEventDec:Decoder[StopExecutionEvent] = deriveDecoder[StopExecutionEvent]
}

case class StopExecutionEvent(signature: EthereumSignature) extends OpenlawVmEvent {
  override def typeIdentifier: String = className[StopExecutionEvent]
  override def serialize: String = this.asJson.noSpaces
}

