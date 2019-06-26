package org.adridadou.openlaw.oracles

import cats.implicits._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm._
import io.circe.{Decoder, Encoder}
import org.adridadou.openlaw.parser.template.VariableName
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.result.{Failure, Result}

case class ResumeContractOracle(crypto:CryptoService) extends OpenlawOracle[ResumeExecutionEvent] {

  override def incoming(vm:OpenlawVm, event: ResumeExecutionEvent): Result[OpenlawVm] = {
    val identityResult = vm
      .getAllVariables(IdentityType)
      .toList
      .map { case (id,variable) => (variable.name, vm.evaluate[Identity](id, variable.name)) }
      .collect {
        // TODO: Figure out which errors are expected here and allow remaining failures to stop processing
        case (id, Right(identity)) =>
          vm.isSignatureValid(vm.contractDefinition.id(crypto).resumeContract(crypto), OpenlawSignatureEvent(vm.contractId, identity.email, "", event.signature, EthereumHash.empty)).map { x =>
            (id, identity) -> x
          }
      }
      .sequence
      .map { list =>
        list.find {
          case (_, boolean) => boolean
        }
      }

    identityResult.flatMap {
      case Some(((name:VariableName, identity:Identity), _)) => processEvent(vm, event, name.name, identity)
      case _ => Failure("invalid event! no matching identity while resuming the contract")
    }
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:ResumeExecutionEvent => true
    case _ => false
  }

  private def processEvent(vm:OpenlawVm, event:ResumeExecutionEvent, name: String, identity:Identity): Result[OpenlawVm] = {
    vm(UpdateExecutionStateCommand(ContractResumed, event))
  }
}

object ResumeExecutionEvent {
  implicit val resumeExecutionEventEnc:Encoder[ResumeExecutionEvent] = deriveEncoder[ResumeExecutionEvent]
  implicit val resumeExecutionEventDec:Decoder[ResumeExecutionEvent] = deriveDecoder[ResumeExecutionEvent]
}

case class ResumeExecutionEvent(signature: EthereumSignature) extends OpenlawVmEvent {
  override def typeIdentifier: String = className[ResumeExecutionEvent]
  override def serialize: String = this.asJson.noSpaces
}

