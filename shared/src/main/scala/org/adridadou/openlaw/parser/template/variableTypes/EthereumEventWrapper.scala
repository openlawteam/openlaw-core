package org.adridadou.openlaw.parser.template.variableTypes

import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser._
import io.circe.syntax._
import org.adridadou.openlaw.oracles.EthereumEventFilterEvent
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import org.adridadou.openlaw.result.Implicits.RichOption

/* Wrapper variable type for ethereum events for use in the event listeners. Should not be able to be created manually in
 * templates, so it left out of the registered varaible types list.
 */
case object EthereumEventWrapper extends VariableType("EthereumEventWrapper") {
  implicit val smartContractEnc: Encoder[EthereumEventFilterEvent] = deriveEncoder[EthereumEventFilterEvent]
  implicit val smartContractDec: Decoder[EthereumEventFilterEvent] = deriveDecoder[EthereumEventFilterEvent]

  override def validateOperation(expr: ValueExpression, executionResult: TemplateExecutionResult): Option[String] = None

  override def cast(value: String, executionResult: TemplateExecutionResult): EthereumEventFilterEvent =
    handleEither(decode[EthereumEventFilterEvent](value))

  override def internalFormat(value: Any): String = value match {
    case call:EthereumEventFilterEvent => call.asJson.noSpaces
  }

  override def access(value: Any, keys: Seq[String], executionResult:TemplateExecutionResult): Result[Any] = keys match {
    case Seq() => Success(value)
    case Seq(x) =>
      val values = VariableType.convert[Map[String, Any]](value)
      values.get(x).toResult(s"field $x not found in event")
    case x => Failure(s"invalid key: $x")
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[_ <: EthereumEventFilterEvent] = classOf[EthereumEventFilterEvent]

  def thisType: VariableType = EthereumEventWrapper
}
