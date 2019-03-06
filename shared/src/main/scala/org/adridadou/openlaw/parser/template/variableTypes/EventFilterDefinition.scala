package org.adridadou.openlaw.parser.template.variableTypes

import java.time.{LocalDateTime, ZoneOffset}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression

case class EventFilterDefinition(
  address: Expression,
  abi: Expression,
  eventType: Expression,
  conditionFilter: Expression) extends ActionValue {

  def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions: Seq[OpenlawExecution]): Option[LocalDateTime] = ???
}
