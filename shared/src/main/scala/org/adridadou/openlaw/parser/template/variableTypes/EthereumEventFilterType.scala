package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser._
import io.circe.syntax._
import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.oracles.EthereumEventFilterExecution
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case object EthereumEventFilterType extends VariableType("EthereumEventFilter") with ActionType {
  implicit val smartContractEnc: Encoder[EventFilterDefinition] = deriveEncoder[EventFilterDefinition]
  implicit val smartContractDec: Decoder[EventFilterDefinition] = deriveDecoder[EventFilterDefinition]

  case class EthereumEventPropertyDef(typeDef:VariableType, data:Seq[EthereumEventFilterExecution] => Result[Option[OpenlawValue]])

  private val propertyDef:Map[String,EthereumEventPropertyDef] = Map[String, EthereumEventPropertyDef](
    "executionDate" -> EthereumEventPropertyDef(typeDef = DateTimeType, evts => Success(evts.headOption.map(_.executionDate))),
    "tx" -> EthereumEventPropertyDef(typeDef = EthTxHashType, evts => Success(evts.headOption.map(_.event.hash)))
  )

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[EventFilterDefinition] =
    handleEither(decode[EventFilterDefinition](value))

  override def internalFormat(value: OpenlawValue): Result[String] = value match {
    case call:EventFilterDefinition => Success(call.asJson.noSpaces)
  }

  private def propertyDef(key:String, expr:Expression, executionResult:TemplateExecutionResult): Result[EthereumEventPropertyDef] =
    expr
      .evaluate(executionResult)
      .flatMap { option =>
        option
          .map {
            case eventFilterDefinition: EventFilterDefinition =>
              eventFilterDefinition
                .abiOpenlawVariables(executionResult)
                .map(list => list.find(_.name.name === key).map(definition => definition.varType(executionResult))) match {
                case Success(Some(typeDef)) =>

                  Success(EthereumEventPropertyDef(typeDef = typeDef, _.headOption.map { execution =>
                    generateStructureType(VariableName("none"), eventFilterDefinition, executionResult).flatMap { structure =>
                      structure.cast(execution.event.values.asJson.noSpaces, executionResult).flatMap(structure.access(_, VariableName("none"), Seq(key), executionResult))
                    }
                  }.getOrElse(Success(None))))
                case Success(None) =>
                  propertyDef.get(key) match {
                    case Some(pd) =>
                      Success(pd)
                    case None =>
                      Failure(s"unknown key $key for $expr")
                  }
                case Failure(ex, message) =>
                  Failure(ex, message)
              }

            case x => Failure(s"unexpected value provided, expected EventFilterDefinition: $x")
          }
          .getOrElse(Failure("the Ethereum event filter definition could not be found"))
      }

  override def keysType(keys: Seq[String], expr: Expression, executionResult: TemplateExecutionResult): Result[VariableType] =
    keys match {
      case Seq(key) =>
        propertyDef(key, expr, executionResult).map(_.typeDef)
      case _ => super.keysType(keys, expr, executionResult)
    }

  override def validateKeys(name:VariableName, keys: Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] = keys match {
    case Seq(key) =>
      propertyDef(key, expression, executionResult).map(_ => Unit)
    case _ =>
      super.validateKeys(name, keys, expression, executionResult)
  }

  override def access(value: OpenlawValue, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    keys.toList match {
      case Nil => Success(Some(value))
      case head::tail if tail.isEmpty =>
        propertyDef(head, name, executionResult)
          .flatMap(_.data(getExecutions(name, executionResult)))

      case _ => Failure(s"Ethereum event only support one level of properties. invalid property access ${keys.mkString(".")}")
    }
  }

  private def getExecutions(name:VariableName, executionResult: TemplateExecutionResult):Result[Seq[EthereumEventFilterExecution]] = {
    executionResult.executions.get(name)
      .map(_.executionMap.values.toList.map(VariableType.convert[EthereumEventFilterExecution]))
      .getOrElse(List.empty)
      .sequence
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[EventFilterDefinition]] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        attempt {
          Some(EventFilterDefinition(
            contractAddress = getExpression(values, "contract address"),
            interface = getExpression(values, "interface"),
            eventType = getExpression(values, "event type name"),
            conditionalFilter = getExpression(values, "conditional filter")
          ))
        }
      case _ =>
        Failure("Ethereum event listener needs to get 'contract address', 'interface', 'event type name', 'conditional filter' as constructor parameter")
    }
  }

  private def generateStructureType(name: VariableName, eventFilter: EventFilterDefinition, executionResult: TemplateExecutionResult): Result[VariableType] = {
    eventFilter.abiOpenlawVariables(executionResult).map(varDefinitions => {
      val typeDefinitions =
        varDefinitions
          .collect { case VariableDefinition(n, Some(typeDef), _, _, _, _) => n -> executionResult.findVariableType(typeDef) }
          .collect { case (n, Some(variableType)) => n -> variableType }

      val structure = Structure(typeDefinitions.toMap, typeDefinitions.map { case(k, _) => k })
      AbstractStructureType.generateType(name, structure)
    })
  }

  override def getTypeClass: Class[_ <: EventFilterDefinition] = classOf[EventFilterDefinition]

  def thisType: VariableType = EthereumEventFilterType

  override def actionValue(value: OpenlawValue): Result[EventFilterDefinition] = VariableType.convert[EventFilterDefinition](value)
}
