package org.adridadou.openlaw.oracles

import java.time.LocalDateTime

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.VariableName
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging
import io.circe.syntax._
import cats.implicits._
import org.adridadou.openlaw.result.{Result, Success}

case class EthereumEventFilterOracle() extends OpenlawOracle[EthereumEventFilterEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: EthereumEventFilterEvent): Result[OpenlawVm] = {
    vm.getAllVariables(EthereumEventFilter).find {
      case (_, variable) => variable.name === event.name //TODO: match as well by the event type and the filter in case many variables with the same name exist
    }.map({
      case (executionResult, variable) =>
        variable.defaultValue.map(EthereumEventFilter.construct(_, executionResult)).map({
          case Success(Some(eventFilter)) =>
            executionResult.startEmbeddedExecution()
            val matchFilter = eventFilter.conditionalFilter.evaluate(executionResult).exists(VariableType.convert[Boolean])
            if(matchFilter) {
              vm.newExecution(event.name, )
            }
          case _ => Success(vm)
        })
    })
  }

  private def toOpenlawExecutionStatus(event:EthereumSmartContractCallEvent):OpenlawExecutionStatus = event match {
    case _:SuccessfulEthereumSmartContractCallEvent =>
      SuccessfulExecution
    case _:PendingEthereumSmartContractCallEvent=>
      PendingExecution
    case _:FailedEthereumSmartContractCallEvent =>
      FailedExecution
  }


  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:EthereumEventFilterEvent => true
    case _ => false
  }
}

object EthereumEventFilterEvent {
  implicit val ethereumEventFilterEventEnc: Encoder[EthereumEventFilterEvent] = deriveEncoder[EthereumEventFilterEvent]
  implicit val ethereumEventFilterEventDec: Decoder[EthereumEventFilterEvent] = deriveDecoder[EthereumEventFilterEvent]
}


final case class EthereumEventFilterEvent(
                                                 name:VariableName,
                                                 hash:EthereumHash,
                                                 smartContractAddress:EthereumAddress,
                                                 eventType:String,
                                                 values:Map[String, Any],
                                                 executionDate:LocalDateTime) extends OpenlawVmEvent {
  override def typeIdentifier: String = className[EthereumEventFilterEvent]
  override def serialize: String = this.asJson.noSpaces
}