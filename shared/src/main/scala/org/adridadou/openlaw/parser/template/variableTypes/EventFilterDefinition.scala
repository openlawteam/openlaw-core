package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import java.time.LocalDateTime

import org.adridadou.openlaw.parser.abi.AbiParser.AbiType
import org.adridadou.openlaw.parser.abi.{AbiEntry, AbiParam, AbiParser}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, Result, attempt}
import org.adridadou.openlaw.result.Implicits.RichOption

case class EventFilterDefinition(
  contractAddress: Expression,
  interface: Expression,
  eventType: Expression,
  conditionalFilter: Expression) extends ActionValue {

  def abiEntries(executionResult: TemplateExecutionResult): Result[List[AbiEntry]] = for {
    interfaceAny <- attempt(interface.evaluate(executionResult)).flatMap(_.toResult(s"expression '$interface' failed to evaluate"))
    interfaceString <- attempt(VariableType.convert[String](interfaceAny))
    entries <- AbiParser.parse(interfaceString)
  } yield entries

  def abiClasses(executionResult: TemplateExecutionResult): Result[List[Class[_]]] = {
    for {
      entries <- abiEntries(executionResult)
      eventTypeAny <- eventType.evaluate(executionResult).toResult("could not evaluate eventType")
      eventTypeString <- attempt(VariableType.convert[String](eventTypeAny))
      entry <- findNameEntry(eventTypeString, entries)
      variables <- convertVariablesScalaTypes(entry)
    } yield variables
  }

  def abiOpenlawVariables(executionResult: TemplateExecutionResult): Result[List[VariableDefinition]] =
    for {
      entries <- abiEntries(executionResult)
      eventTypeAny <- eventType.evaluate(executionResult).toResult("could not evaluate eventType")
      eventTypeString <- attempt(VariableType.convert[String](eventTypeAny))
      entry <- findNameEntry(eventTypeString, entries)
      variables <- convertVariablesOpenlawType(entry)
    } yield variables

  def abiScalaVariables(executionResult: TemplateExecutionResult): Result[List[Class[_]]] =
    for {
      entries <- abiEntries(executionResult)
      eventTypeAny <- eventType.evaluate(executionResult).toResult("could not evaluate eventType")
      eventTypeString <- attempt(VariableType.convert[String](eventTypeAny))
      entry <- findNameEntry(eventTypeString, entries)
      variables <- convertVariablesScalaTypes(entry)
    } yield variables

  def findNameEntry(targetName: String, entries: List[AbiEntry]): Result[AbiEntry] =
    entries
      .collect { case entry @ AbiEntry(Some(name), _, _, _, _, _, _, _, _) => (name, entry) }
      .find { case (entryName, entry) => entryName === targetName }
      .map { case (_, entry) => entry }
      .toResult(s"no entry found for event named $targetName")

  def convertVariablesOpenlawType(entry: AbiEntry): Result[List[VariableDefinition]] =
    entry.inputs.getOrElse(Nil) ++ entry.outputs.getOrElse(Nil) match {
      case Nil => Failure("no inputs or outputs in ABI entry")
      case list => list.map(convertVariableOpenlawType).sequence
    }

  def convertVariablesScalaTypes(entry: AbiEntry): Result[List[Class[_]]] =
    entry.inputs.getOrElse(Nil) ++ entry.outputs.getOrElse(Nil) match {
      case Nil => Failure("no inputs or outputs in ABI entry")
      case list => list.map(convertVariableScalaTypes).sequence
    }

  def convertVariableOpenlawType(param: AbiParam): Result[VariableDefinition] =
    attempt(AbiType.withName(param.`type`)).map { variableType =>
      VariableDefinition(VariableName(param.name), Some(VariableTypeDefinition(variableType.openlawType.name)))
    }

  def convertVariableScalaTypes(param: AbiParam): Result[Class[_]] =
    attempt(AbiType.withName(param.`type`)).map { variableType => variableType.scalaType }

  def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions: Seq[OpenlawExecution]): Option[LocalDateTime] = {
    pastExecutions match {
      case Seq() => Some(LocalDateTime.now)
      case _ => None
    }
  }
}
