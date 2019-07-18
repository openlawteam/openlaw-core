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
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.result.Implicits._
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

  implicit val integratedServiceDefinitionEnc:Encoder[IntegratedServiceDefinition] = deriveEncoder
  implicit val integratedServiceDefinitionDec:Decoder[IntegratedServiceDefinition] = deriveDecoder
  implicit val integratedServiceDefinitionEq:Eq[IntegratedServiceDefinition] = Eq.fromUniversalEquals
}

object ServiceName {
  implicit val serviceNameEnc:Encoder[ServiceName] = (sn: ServiceName) => Json.fromString(sn.serviceName)
  implicit val serviceNameDec:Decoder[ServiceName] = (c: HCursor) =>  c.as[String].map(ServiceName(_))
  implicit val serviceNameKeyEnc:KeyEncoder[ServiceName] = (key: ServiceName) => key.serviceName
  implicit val serviceNameKeyDec:KeyDecoder[ServiceName] = (key: String) => Some(ServiceName(key))

  implicit val serviceNameEq:Eq[ServiceName] = Eq.fromUniversalEquals

  val openlawServiceName = ServiceName("Openlaw")
}

case class ServiceName(serviceName:String)

case class IntegratedServiceDefinition(input:Structure, output:Structure) {
  def definedInput: DefinedStructureType = DefinedStructureType(input, "Input")
  def definedOutput: DefinedStructureType = DefinedStructureType(output, "Output")
}

case class SignatureServiceDefinition() {
  val definition =
    """
      |[[Input:Structure(signerEmail: Text; signerFullName: Text; contractContentBase64: Text; contractTitle: Text)]]
      |
      |[[Output:Structure(signerEmail: Text; signerFullName: Text; signature: Text)]]
    """.stripMargin.trim
  val abi = IntegratedServiceDefinition(definition).getOrThrow()
  def definedInput: DefinedStructureType = DefinedStructureType(abi.input, "Input")
  def definedOutput: DefinedStructureType = DefinedStructureType(abi.output, "Output")
}

object SignatureServiceDefinition {
  implicit val signatureServiceDefinitionEnc:Encoder[SignatureServiceDefinition] = deriveEncoder
  implicit val signatureServiceDefinitionDec:Decoder[SignatureServiceDefinition] = deriveDecoder
  implicit val signatureServiceDefinitionEq:Eq[SignatureServiceDefinition] = Eq.fromUniversalEquals
}

case class ExternalCall(serviceName: Expression,
                        parameters: Map[VariableName, Expression],
                        startDate: Option[Expression],
                        endDate: Option[Expression],
                        every: Option[Expression]) extends ActionValue with OpenlawNativeValue {

  override def identifier(executionResult: TemplateExecutionResult): Result[ActionIdentifier] =
    serviceName.evaluateT[OpenlawString](executionResult).flatMap { valueOption =>
      parameters
        .toList
        .sortBy { case (key, _) => key.name }
        .map { case (key, v) => v.evaluate(executionResult).map(option => option.map(key.name + "->" + _).getOrElse("")) }
        .sequence
        .map { values =>
          val value = valueOption.getOrElse("") + "#" + values.mkString("#")
          ActionIdentifier(value)
        }
    }

  def getServiceName(executionResult: TemplateExecutionResult): Result[String] =
    getString(serviceName, executionResult)

  def getParameters(executionResult: TemplateExecutionResult): Result[Map[VariableName, OpenlawValue]] =
    parameters
      .map { case (name, expr) => expr.evaluate(executionResult).map(name -> _) }
      .toList
      .sequence
      .map(_.collect { case (name, Some(value)) => name -> value }.toMap)

  def getStartDate(executionResult: TemplateExecutionResult): Result[Option[LocalDateTime]] =
    startDate.map(getDate(_, executionResult).map(_.underlying)).sequence

  def getEndDate(executionResult: TemplateExecutionResult): Result[Option[LocalDateTime]] =
    endDate.map(getDate(_, executionResult).map(_.underlying)).sequence

  def getEvery(executionResult: TemplateExecutionResult): Result[Option[Period]] =
    every.map(getPeriod(_, executionResult)).sequence

  override def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions: Seq[OpenlawExecution]): Result[Option[LocalDateTime]] = {

    for {
      executions <- pastExecutions.toList.map(VariableType.convert[ExternalCallExecution]).sequence
      result <- {
        val callToRerun: Option[LocalDateTime] = executions
          .find { execution =>
            execution.executionStatus match {
              case FailedExecution =>
                execution.executionDate
                  .isBefore(LocalDateTime.now(executionResult.clock).minus(5, ChronoUnit.MINUTES))
              case _ =>
                false
            }
          }
          .map(_.scheduledDate)

        callToRerun.map(Success(_)).orElse {
          executions.map(_.scheduledDate) match {
            case Nil =>
              Some(getStartDate(executionResult).map(_.getOrElse(LocalDateTime.now(executionResult.clock))))
            case list =>
              val lastDate = list.maxBy(_.toEpochSecond(ZoneOffset.UTC))
              (for {
                schedulePeriodOption <- getEvery(executionResult)
                endDate <- getEndDate(executionResult)
              } yield {
                schedulePeriodOption.map { schedulePeriod =>
                  DateTimeType
                    .plus(Some(OpenlawDateTime(lastDate)), Some(schedulePeriod), executionResult)
                    .flatMap { p =>
                      p
                        .map(VariableType.convert[OpenlawDateTime](_).map(_.underlying))
                        .sequence
                        .map(pOption => pOption.filter(nextDate => endDate.forall(date => nextDate.isBefore(date) || nextDate === date)))
                    }
                }
                  .flatMap(_.sequence)
              }).sequence.map(_.flatten)
          }
        }.sequence
      }
    } yield result
  }
}

object ExternalCall {
  implicit val externalCallEnc: Encoder[ExternalCall] = deriveEncoder[ExternalCall]
  implicit val externalCallDec: Decoder[ExternalCall] = deriveDecoder[ExternalCall]
}
