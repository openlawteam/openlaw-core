package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser._
import io.circe.syntax._
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

  override def access(value: Any, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Any] = {
    keys.toList match {
      case Nil => Success(value)
      case head::tail if tail.isEmpty =>
        value match {
          case eventFilterDefinition: EventFilterDefinition =>
            eventFilterDefinition
              .abiOpenlawVariables(executionResult)
              .map(list => list.find(_.name.name === head).toResult(s"failed to find event field named $head"))
              .flatten
              .map(definition => definition.evaluate(executionResult))
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

  override def getTypeClass: Class[_ <: EventFilterDefinition] = classOf[EventFilterDefinition]

  def thisType: VariableType = EthereumEventFilterType

  override def actionValue(value: Any): EventFilterDefinition = VariableType.convert[EventFilterDefinition](value)
}
