package org.adridadou.openlaw.oracles

import java.time.LocalDateTime

import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import cats.implicits._
import io.circe.{Decoder, Encoder}
import org.adridadou.openlaw.parser.template.{OpenlawTemplateLanguageParserService, TemplateExecutionResult, VariableDefinition, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Result, Success}
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging
import VariableName._
import LocalDateTimeHelper._



case class EthereumEventFilterExecution(executionDate: LocalDateTime, key: Any, executionStatus: OpenlawExecutionStatus = PendingExecution) extends OpenlawExecution {
  val scheduledDate: LocalDateTime = executionDate
}

case class EthereumEventFilterOracle(parser: OpenlawTemplateLanguageParserService) extends OpenlawOracle[EthereumEventFilterEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: EthereumEventFilterEvent): Result[OpenlawVm] = {
    vm
      .getAllVariables(EthereumEventFilterType)
      .find { case (_, variable) => variable.name === event.name }.flatMap {
      case (executionResult, variable) =>
        variable
          .defaultValue
          .map(EthereumEventFilterType.construct(_, executionResult))
          .map {
            case Success(Some(eventFilter)) =>
              val name = VariableName("this")
              val result = for {
                structureType <- generateStructureType(name, eventFilter, executionResult)
                child <- executionResult.startEphemeralExecution(name, structureType.cast(event.values.asJson.noSpaces, executionResult), structureType)
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

  private def generateStructureType(name: VariableName, eventFilter:EventFilterDefinition, executionResult: TemplateExecutionResult): Result[VariableType] = {
    eventFilter.abiOpenlawVariables(executionResult).map(varDefinitions => {
      val typeDefinitions =
        varDefinitions
          .collect { case VariableDefinition(n, Some(typeDef), _, _, _, _) => n -> executionResult.findVariableType(typeDef) }
          .collect { case (n, Some(variableType)) => n -> variableType }

      val structure = Structure(typeDefinitions.toMap, typeDefinitions.map { case(k, _) => k })
      AbstractStructureType.generateType(name, structure)
    })
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
  values:Map[VariableName, String],
  executionDate:LocalDateTime) extends OpenlawVmEvent {

  override def typeIdentifier: String = className[EthereumEventFilterEvent]
  override def serialize: String = this.asJson.noSpaces
}