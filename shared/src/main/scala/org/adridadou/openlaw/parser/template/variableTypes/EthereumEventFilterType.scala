package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser._
import io.circe.syntax._
import org.adridadou.openlaw.oracles.EthereumEventFilterExecution
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import org.adridadou.openlaw.result.Implicits.RichOption

case object EthereumEventFilterType extends VariableType("EthereumEventFilter") with ActionType {
  implicit val smartContractEnc: Encoder[EventFilterDefinition] = deriveEncoder[EventFilterDefinition]
  implicit val smartContractDec: Decoder[EventFilterDefinition] = deriveDecoder[EventFilterDefinition]

  override def cast(value: String, executionResult: TemplateExecutionResult): EventFilterDefinition =
    handleEither(decode[EventFilterDefinition](value))

  override def internalFormat(value: Any): String = value match {
    case call:EventFilterDefinition =>
      call.asJson.noSpaces
  }

  override def keysType(keys: Seq[String], expr: Expression, executionResult: TemplateExecutionResult): Result[VariableType] =
    keys match {
      case Seq(key) =>
        expr.evaluate(executionResult).map {
            case eventFilterDefinition: EventFilterDefinition =>
              eventFilterDefinition
                .abiOpenlawVariables(executionResult)
                .map(list => list.find(_.name.name === key).toResult(s"failed to find event field named $key"))
                .flatten
                .map(definition => definition.varType(executionResult))
            case x => Failure(s"unexpected value provided, expected EventFilterDefinition: $x")
          }.getOrElse(Failure("the ethereum event filter definition could not be found"))
      case _ => super.keysType(keys, expr, executionResult)
    }

  override def validateKeys(name:VariableName, keys: Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] = keys match {
    case Seq(key) =>
      expression.evaluate(executionResult).map {
        case eventFilterDefinition: EventFilterDefinition =>
          eventFilterDefinition
            .abiOpenlawVariables(executionResult)
            .map(list => list.find(_.name.name === key).toResult(s"failed to find event field named $key"))
            .flatten
            .map(definition => definition.varType(executionResult))
        case x => Failure(s"unexpected value provided, expected EventFilterDefinition: $x")
      }.map(_ => Success()).getOrElse(Failure("the ethereum event filter definition could not be found"))
    case _ => super.validateKeys(name, keys, expression, executionResult)
  }

  override def access(value: Any, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[Any]] = {
    keys.toList match {
      case Nil => Success(Some(value))
      case _::tail if tail.isEmpty =>
        value match {
          case eventFilter: EventFilterDefinition =>

            executionResult
              .executions
              .get(name)
              .map { executions =>
                executions.executionMap.values.headOption match {
                  case Some(execution: EthereumEventFilterExecution) =>
                    generateStructureType(VariableName("none"), eventFilter, executionResult).flatMap { structure =>
                      structure.access(structure.cast(execution.event.values.asJson.noSpaces, executionResult), name, keys, executionResult)
                    }
                  case Some(other) =>
                    Failure(s"the execution type should be EthereumEventFilterExecution but was ${other.getClass.getSimpleName} instead")
                  case None =>
                    Success(None)
                }
              }
              .getOrElse(Success(None))

          case x => Failure(s"unexpected value provided, expected EventFilterDefinition: $x")
        }

      case _ => Failure(s"Ethereum event only support one level of properties. invalid property access ${keys.mkString(".")}")
    }
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

  override def actionValue(value: Any): EventFilterDefinition = VariableType.convert[EventFilterDefinition](value)
}
