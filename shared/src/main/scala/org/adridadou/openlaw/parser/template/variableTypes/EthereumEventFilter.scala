package org.adridadou.openlaw.parser.template.variableTypes

import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser._
import io.circe.syntax._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, attempt}

case object EthereumEventFilter extends VariableType("EthereumEventFilter") with ActionType {
  implicit val smartContractEnc: Encoder[EventFilterDefinition] = deriveEncoder[EventFilterDefinition]
  implicit val smartContractDec: Decoder[EventFilterDefinition] = deriveDecoder[EventFilterDefinition]

  override def validateOperation(expr: ValueExpression, executionResult: TemplateExecutionResult): Option[String] = {
    None
  }

  override def cast(value: String, executionResult: TemplateExecutionResult): EventFilterDefinition =
    handleEither(decode[EventFilterDefinition](value))

  override def internalFormat(value: Any): String = value match {
    case call:EventFilterDefinition =>
      call.asJson.noSpaces
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

  def thisType: VariableType = EthereumEventFilter

  private def getExpressionList(param:Parameter):Seq[Expression] = param match {
    case ListParameter(exprs) => exprs
    case OneValueParameter(expr) => Seq(expr)
    case _ => throw new RuntimeException("invalid parameter type " + param.getClass.getSimpleName + " expecting list of expressions")
  }

  override def actionValue(value: Any): EventFilterDefinition = VariableType.convert[EventFilterDefinition](value)
}
