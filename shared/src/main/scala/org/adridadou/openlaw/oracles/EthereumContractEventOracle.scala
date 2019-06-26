package org.adridadou.openlaw.oracles

import java.time.LocalDateTime

import cats.implicits._
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json}
import org.adridadou.openlaw.parser.template.{ActionIdentifier, OpenlawTemplateLanguageParserService, TemplateExecutionResult, VariableDefinition, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Result, Success}
import org.adridadou.openlaw.vm.{OpenlawVm, OpenlawVmEvent}
import slogging.LazyLogging
import VariableName._
import LocalDateTimeHelper._
import org.adridadou.openlaw.{OpenlawBoolean, OpenlawString}

object EthereumEventFilterExecution {
  implicit val ethEventFilterExecutionEnc:Encoder[EthereumEventFilterExecution] = deriveEncoder[EthereumEventFilterExecution]
  implicit val ethEventFilterExecutionDec:Decoder[EthereumEventFilterExecution] = deriveDecoder[EthereumEventFilterExecution]
}

case class EthereumEventFilterExecution(executionDate: LocalDateTime, executionStatus: OpenlawExecutionStatus = PendingExecution, event:EthereumEventFilterEvent) extends OpenlawExecution {
  val scheduledDate: LocalDateTime = executionDate

  override def key: Any = event.hash

  override def typeIdentifier: String = className[EthereumEventFilterExecution]

  override def serialize: Json = this.asJson
}

case class EthereumEventFilterOracle(parser: OpenlawTemplateLanguageParserService, crypto:CryptoService) extends OpenlawOracle[EthereumEventFilterEvent] with LazyLogging {

  override def incoming(vm: OpenlawVm, event: EthereumEventFilterEvent): Result[OpenlawVm] = {
    vm.getAllVariableValues[EventFilterDefinition](EthereumEventFilterType).flatMap { values =>
      values
        .toList
        .map { case (eventFilterDefinition, executionResult) => eventFilterDefinition.identifier(executionResult).map((eventFilterDefinition, executionResult, _)) }
        .sequence
        .flatMap { list =>
          list
            .find { case (_, _, id) => id === event.identifier }
            .map {
              case (eventFilterDefinition, executionResult, _) =>
                val name = VariableName("this")
                val result = for {
                  structureType <- generateStructureType(name, eventFilterDefinition, executionResult)
                  cast <- structureType.cast(event.values.asJson.noSpaces, executionResult)
                  child <- executionResult.startEphemeralExecution(name, cast, structureType)
                  addressOption <- eventFilterDefinition.contractAddress.evaluate(child)
                  eventTypeOption <- eventFilterDefinition.eventType.evaluate(child)
                  conditionalOption <- eventFilterDefinition.conditionalFilter.evaluate(child)
                } yield {
                  (addressOption, eventTypeOption, conditionalOption) match {
                    case (Some(OpenlawString(address)), Some(OpenlawString(eventType)), Some(OpenlawBoolean(true))) if address === event.smartContractAddress.withLeading0x && eventType === event.eventType =>
                      val execution = EthereumEventFilterExecution(event.executionDate, SuccessfulExecution, event)
                      Success(vm.newExecution(event.identifier, execution))
                    case _ =>
                      Success(vm)
                  }
                }
                result.flatten
              case _ => Success(vm)
            } match {
              case Some(newVm) => newVm
              case None =>
                Success(vm)
            }
        }
    }
  }

  private def generateStructureType(name: VariableName, eventFilter:EventFilterDefinition, executionResult: TemplateExecutionResult): Result[VariableType] = {
    eventFilter.abiOpenlawVariables(executionResult).map(varDefinitions => {
      val definitions = varDefinitions.map(definition => definition.name -> definition)
      val types = varDefinitions.map(definition => definition.name -> definition.varType(executionResult))

      val structure = Structure(typeDefinition = definitions.toMap, types = types.toMap, names = definitions.map { case(k, _) => k })
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
     identifier:ActionIdentifier,
     hash:EthereumHash,
     smartContractAddress:EthereumAddress,
     eventType:String,
     values:Map[VariableName, String],
     executionDate:LocalDateTime) extends OpenlawVmEvent {

  override def typeIdentifier: String = className[EthereumEventFilterEvent]
  override def serialize: String = this.asJson.noSpaces
}