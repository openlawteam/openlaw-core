package org.adridadou.openlaw.parser.template.variableTypes

import java.time.{LocalDateTime, ZoneOffset}

import org.adridadou.openlaw.parser.abi.{AbiEntry, AbiParser}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{attempt, Result}
import org.adridadou.openlaw.result.Implicits.RichOption

case class EventFilterDefinition(
  contractAddress: Expression,
  interface: Expression,
  eventType: Expression,
  conditionalFilter: Expression) extends ActionValue {

  def abiEntries(executionResult: TemplateExecutionResult): Result[List[AbiEntry]] = for {
    interfaceAny <- attempt(interface.evaluate(executionResult)).map(_.toResult(s"expression '$interface' failed to evaluate"))
    interfaceString <- attempt(VariableType.convert[String](interfaceAny))
    entries <- AbiParser.parse(interfaceString)
  } yield entries

  def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions: Seq[OpenlawExecution]): Option[LocalDateTime] = ???
}
