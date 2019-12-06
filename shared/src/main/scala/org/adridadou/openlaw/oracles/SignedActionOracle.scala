package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.VariableName
import org.adridadou.openlaw.parser.template.variableTypes.{EthereumData, EthereumHash, EthereumSignature, ExternalSignature, ExternalSignatureType, Identity, IdentityType}
import org.adridadou.openlaw.result.{Failure, Result}
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import cats.implicits._

trait SignedActionEvent extends OpenlawVmEvent {
	def signature:EthereumSignature
}

trait SignedActionOracle[R <: SignedActionEvent] extends OpenlawOracle[R]{

	def checkAction(vm:OpenlawVm, crypto:CryptoService, event:R, actionData:EthereumData): Result[OpenlawVm] = {
		val identityResult = vm
			.getAllVariables(IdentityType)
			.toList
			.map { case (executionResult,variable) => (variable.name, executionResult.evaluate[Identity](variable.name)) }
			.collect {
				case (id, Right(identity)) =>
					vm.isSignatureValid(vm.contractDefinition.id(crypto).resumeContract(crypto), OpenlawSignatureEvent(vm.contractId, identity.email, "", event.signature, EthereumHash.empty)).map { x =>
						(id, identity) -> x
					}
				case (_, Failure(ex, message)) =>
					Failure(ex, message)
			}
			.sequence
			.map { list =>
				list.find {
					case (_, boolean) => boolean
				}
			}

		val externalSignatureResult = vm
			.getAllVariables(ExternalSignatureType)
			.toList
			.map { case (executionResult,variable) => (variable.name, executionResult.evaluate[ExternalSignature](variable.name)) }
			.collect {
				case (id, Right(ExternalSignature(Some(identity), serviceName))) =>
					vm.isSignatureValid(vm.contractDefinition.id(crypto).resumeContract(crypto), ExternalSignatureEvent(vm.contractId, identity.email, "", serviceName, event.signature)).map { x =>
						(id, identity) -> x
					}
				case (_, Failure(ex, message)) =>
					Failure(ex, message)
			}
			.sequence
			.map { list =>
				list.find {
					case (_, boolean) => boolean
				}
			}

		identityResult.flatMap {
			case Some(((name:VariableName, identity:Identity), _)) => processEvent(vm, event, name.name, identity)
			case _ =>
				externalSignatureResult.flatMap {
					case Some(((name:VariableName, identity:Identity), _)) => processEvent(vm, event, name.name, identity)
					case _ => Failure("invalid event! no matching identity while resuming the contract")
				}
		}
	}

	def processEvent(vm:OpenlawVm, event:R, name: String, identity:Identity): Result[OpenlawVm]

}
