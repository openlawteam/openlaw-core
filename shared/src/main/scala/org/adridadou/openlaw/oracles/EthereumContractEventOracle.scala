package org.adridadou.openlaw.oracles

import java.time.{Duration, LocalDate, LocalDateTime}

import cats.implicits._
import io.circe._
import io.circe.java8.time._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import org.adridadou.openlaw.parser.template.{OpenlawTemplateLanguageParserService, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Result, Success}
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging

case class EthereumEventFilterOracle(val parser: OpenlawTemplateLanguageParserService) extends OpenlawOracle[EthereumEventFilterEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: EthereumEventFilterEvent): Result[OpenlawVm] = {
    vm
      .getAllVariables(EthereumEventFilter)
      .find { case (_, variable) => variable.name === event.name } //TODO: match as well by the event type and the filter in case many variables with the same name exist
      .map {
        case (executionResult, variable) =>
          variable
            .defaultValue
            .map(EthereumEventFilter.construct(_, executionResult))
            .map {
              case Success(Some(eventFilter)) =>
                val anonymous = executionResult.createAnonymousVariable()
                val result = for {
                  template <- parser.compileTemplate("")
                  child <- executionResult.startEmbeddedExecution(anonymous, template, VariableName("this"), event.values, EthereumEventWrapper)
                } yield {
                  val matchFilter = eventFilter.conditionalFilter.evaluate(child).exists(VariableType.convert[Boolean])
                  if (matchFilter) {
                    Success(vm.newExecution(event.name, null))
                  } else {
                    Success(vm)
                  }
                }
                result.flatten
              case _ => Success(vm)
            }
      }
      .flatten
      .getOrElse(Success(vm))
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
  implicit val variableNameEnc = Encoder[VariableName]
  implicit val ethereumHashEnc = Encoder[EthereumHash]
  implicit val variableNameDec = Decoder[VariableName]
  implicit val ethereumHashDec = Decoder[EthereumHash]
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