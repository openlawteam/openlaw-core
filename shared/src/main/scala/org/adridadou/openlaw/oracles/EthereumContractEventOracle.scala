package org.adridadou.openlaw.oracles

import java.time.LocalDateTime

import cats.implicits._
import org.adridadou.openlaw.parser.template.{OpenlawTemplateLanguageParserService, TemplateExecutionResult, VariableDefinition, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Result, Success}
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging

case class EthereumEventFilterExecution(executionDate: LocalDateTime, key: Any, executionStatus: OpenlawExecutionStatus = PendingExecution) extends OpenlawExecution {
  val scheduledDate = executionDate
}

case class EthereumEventFilterOracle(parser: OpenlawTemplateLanguageParserService) extends OpenlawOracle[EthereumEventFilterEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: EthereumEventFilterEvent): Result[OpenlawVm] = {
    vm
      .getAllVariables(EthereumEventFilter)
      .find { case (_, variable) => variable.name === event.name }.flatMap {
      case (executionResult, variable) =>
        variable
          .defaultValue
          .map(EthereumEventFilter.construct(_, executionResult))
          .map {
            case Success(Some(eventFilter)) =>
              val name = VariableName("this")
              val result = for {
                variables <- eventFilter.abiOpenlawVariables(executionResult)
                child <- executionResult.startEphemeralExecution(name, event.values, generateStructureType(name, variables, executionResult))
              } yield {

                val matchFilter = eventFilter.conditionalFilter.evaluate(child)
                if (matchFilter.exists(VariableType.convert[Boolean])) {
                 val execution = EthereumEventFilterExecution(event.executionDate, event.hash)
                  Success(vm.newExecution(event.name, execution))
                } else {
                  Success(vm)
                }
              }
              result.flatten
            case _ => Success(vm)
          }
    }
      .getOrElse(Success(vm))
  }

  private def generateStructureType(name: VariableName, varDefinitions: List[VariableDefinition], executionResult: TemplateExecutionResult): VariableType = {
    val typeDefinitions =
      varDefinitions
        .collect { case VariableDefinition(n, Some(typeDef), _, _, _, _) => n -> executionResult.findVariableType(typeDef) }
        .collect { case (n, Some(variableType)) => n -> variableType }

    val structure = Structure(typeDefinitions.toMap, typeDefinitions.map { case(k, _) => k })
    AbstractStructureType.generateType(name, structure)
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:EthereumEventFilterEvent => true
    case _ => false
  }
}

object EthereumEventFilterEvent {
  /*
  implicit val variableNameEnc = Encoder[VariableName]
  implicit val ethereumHashEnc = Encoder[EthereumHash]
  implicit val variableNameDec = Decoder[VariableName]
  implicit val ethereumHashDec = Decoder[EthereumHash]
  implicit val ethereumEventFilterEventEnc: Encoder[EthereumEventFilterEvent] = deriveEncoder[EthereumEventFilterEvent]
  implicit val ethereumEventFilterEventDec: Decoder[EthereumEventFilterEvent] = deriveDecoder[EthereumEventFilterEvent]
  */
}

final case class EthereumEventFilterEvent(
  name:VariableName,
  hash:EthereumHash,
  smartContractAddress:EthereumAddress,
  eventType:String,
  values:Map[VariableName, Any],
  executionDate:LocalDateTime) extends OpenlawVmEvent {

  override def typeIdentifier: String = className[EthereumEventFilterEvent]
  override def serialize: String = ""//this.asJson.noSpaces
}