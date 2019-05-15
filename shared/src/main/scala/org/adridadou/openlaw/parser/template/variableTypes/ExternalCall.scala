package org.adridadou.openlaw.parser.template.variableTypes

import java.time.temporal.ChronoUnit
import java.time.{Clock, LocalDateTime, ZoneOffset}

import cats.implicits._
import cats.kernel.Eq
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe._
import org.adridadou.openlaw.{OpenlawDateTime, OpenlawNativeValue, OpenlawString, OpenlawValue, result}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.LocalDateTimeHelper._
import org.adridadou.openlaw.parser.template.variableTypes.VariableType._
import org.adridadou.openlaw.result.{Failure, Success}
import org.adridadou.openlaw.vm.OpenlawExecutionEngine

object IntegratedServiceDefinition {
  val parser = new OpenlawTemplateLanguageParserService(Clock.systemUTC())
  val engine = new OpenlawExecutionEngine()

  def apply(definition:String):result.Result[IntegratedServiceDefinition] = {
    for {
      i <- getStructure(definition, "Input")
      o <- getStructure(definition, "Output")
    } yield new IntegratedServiceDefinition(i, o)
  }

  private def getStructure(input:String, variableTypeName:String):result.Result[Structure] = {
    parser.compileTemplate(input)
      .flatMap(template => engine.execute(template))
      .map(_.findVariableType(VariableTypeDefinition(variableTypeName)))
      .flatMap({
        case Some(t:DefinedStructureType) => Success(t.structure)
        case Some(_) => Failure("invalid type")
        case None => Failure(s"no type found named $variableTypeName")
      })
  }

  implicit val integratedServiceDefinitionEnc:Encoder[IntegratedServiceDefinition] = deriveEncoder[IntegratedServiceDefinition]
  implicit val integratedServiceDefinitionDec:Decoder[IntegratedServiceDefinition] = deriveDecoder[IntegratedServiceDefinition]
  implicit val integratedServiceDefinitionEq:Eq[IntegratedServiceDefinition] = Eq.fromUniversalEquals
}

object ServiceName {
  implicit val serviceNameEnc:Encoder[ServiceName] = (sn: ServiceName) => Json.fromString(sn.serviceName)
  implicit val serviceNameDec:Decoder[ServiceName] = (c: HCursor) =>  for { name <- c.downField("serviceName").as[String] } yield ServiceName(name)
  implicit val serviceNameKeyEnc:KeyEncoder[ServiceName] = (key: ServiceName) => key.serviceName
  implicit val serviceNameKeyDec:KeyDecoder[ServiceName] = (key: String) => Some(ServiceName(key))

  implicit val serviceNameEq:Eq[ServiceName] = Eq.fromUniversalEquals
}

case class ServiceName(serviceName:String)

case class IntegratedServiceDefinition(input:Structure, output:Structure) {
  def definedInput: DefinedStructureType = DefinedStructureType(input, "Input")
  def definedOutput: DefinedStructureType = DefinedStructureType(output, "Output")
}

case class ExternalCall(serviceName: Expression,
                        parameters: Map[VariableName, Expression],
                        startDate: Option[Expression],
                        endDate: Option[Expression],
                        every: Option[Expression]) extends ActionValue with OpenlawNativeValue {

  override def identifier(executionResult: TemplateExecutionResult): ActionIdentifier = {
    val value = serviceName.evaluateT[OpenlawString](executionResult).getOrElse("") + "#" +
    parameters.toSeq
      .sortBy({case (key,_) => key.name})
        .map({case (key, v) => v.evaluate(executionResult).map(key.name + "->" + _).getOrElse("")})
        .mkString("#")

      ActionIdentifier(value)
  }

  def getServiceName(executionResult: TemplateExecutionResult): String =
    getString(serviceName, executionResult)

  def getParameters(executionResult: TemplateExecutionResult): Map[VariableName, OpenlawValue] =
    parameters.flatMap({
      case (name, expr) => expr.evaluate(executionResult).map(name -> _)
    })

  def getStartDate(executionResult: TemplateExecutionResult): Option[LocalDateTime] =
    startDate.map(getDate(_, executionResult).underlying)

  def getEndDate(executionResult: TemplateExecutionResult): Option[LocalDateTime] =
    endDate.map(getDate(_, executionResult).underlying)

  def getEvery(executionResult: TemplateExecutionResult): Option[Period] =
    every.map(getPeriod(_, executionResult))

  override def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions: Seq[OpenlawExecution]): Option[LocalDateTime] = {
    val executions = pastExecutions.map(VariableType.convert[ExternalCallExecution])
    val callToReRun = executions
      .map(VariableType.convert[ExternalCallExecution])
      .find(execution => execution.executionStatus match {
        case FailedExecution =>
          execution.executionDate
            .isBefore(LocalDateTime.now(executionResult.clock).minus(5, ChronoUnit.MINUTES))
        case _ =>
          false
      }).map(_.scheduledDate)

    callToReRun orElse {
      executions.map(_.scheduledDate).toList match {
        case Nil =>
          Some(getStartDate(executionResult).getOrElse(LocalDateTime.now(executionResult.clock)))
        case list =>
          val lastDate = list.maxBy(_.toEpochSecond(ZoneOffset.UTC))
          getEvery(executionResult).flatMap(schedulePeriod => {
            DateTimeType
              .plus(Some(lastDate), Some(schedulePeriod), executionResult)
              .map(VariableType.convert[OpenlawDateTime](_).underlying)
              .filter(nextDate => getEndDate(executionResult).forall(date => nextDate.isBefore(date) || nextDate === date))
          })
      }
    }
  }

}

object ExternalCall {
  implicit val externalCallEnc: Encoder[ExternalCall] = deriveEncoder[ExternalCall]
  implicit val externalCallDec: Decoder[ExternalCall] = deriveDecoder[ExternalCall]
}
