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
    "tx" -> EthereumEventPropertyDef(typeDef = EthTxHashType, evts => Success(evts.headOption.map(_.event.hash))),
    "received" -> EthereumEventPropertyDef(typeDef = YesNoType, evts => Success(Some(evts.nonEmpty)))
  )

  override def cast(value: String, executionResult: TemplateExecutionResult): EventFilterDefinition =
    handleEither(decode[EventFilterDefinition](value))

  override def internalFormat(value: OpenlawValue): String = value match {
    case call:EventFilterDefinition =>
      call.asJson.noSpaces
  }

  private def propertyDef(key:String, expr:Expression, executionResult:TemplateExecutionResult):Result[EthereumEventPropertyDef] = {
    expr.evaluate(executionResult).map {
      case eventFilterDefinition: EventFilterDefinition =>
        eventFilterDefinition
          .abiOpenlawVariables(executionResult)
          .map(list => list.find(_.name.name === key)
          .map(definition => definition.varType(executionResult))) match {
          case Success(Some(typeDef)) =>
            Success(EthereumEventPropertyDef(typeDef = typeDef, _.headOption.map(execution => {
              generateStructureType(VariableName("none"), eventFilterDefinition, executionResult).flatMap { structure =>
                structure.access(structure.cast(execution.event.values.asJson.noSpaces, executionResult), VariableName("none"), Seq(key), executionResult)
              }
            }).getOrElse(Success(None))))
          case Success(None) =>
            Failure(s"property $key not found in the event definition")
          case Failure(ex, message) =>
            Failure(ex, message)
        }

      case x => Failure(s"unexpected value provided, expected EventFilterDefinition: $x")
    }.getOrElse(Failure("the Ethereum event filter definition could not be found"))
  }

  override def keysType(keys: Seq[String], expr: Expression, executionResult: TemplateExecutionResult): Result[VariableType] =
    keys.toList match {
      case key::Nil =>
        propertyDef.get(key) map { e => Success(e.typeDef) } getOrElse Failure(s"unknown key $key for $name")
      case "event"::head::Nil =>
        propertyDef(head, expr, executionResult).map(_.typeDef)
      case _ => super.keysType(keys, expr, executionResult)
    }

  override def validateKeys(name:VariableName, keys: Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] = keys.toList match {
    case key::Nil =>
      propertyDef.get(key) map { _ => Success.unit } getOrElse Failure(s"unknown key $key for $name")
    case "event"::head::Nil =>
      propertyDef(head, name, executionResult).map(_ => Unit)
    case _ =>
      super.validateKeys(name, keys, expression, executionResult)
  }

  override def access(value: OpenlawValue, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    val actionDefinition = VariableType.convert[EventFilterDefinition](value)
    val identifier = actionDefinition.identifier(executionResult)
    keys.toList match {
      case Nil => Success(Some(value))
      case key::Nil =>
        propertyDef.get(key) match {
          case Some(pd) =>
            pd.data(getExecutions(identifier, executionResult))
          case None =>
            Failure(s"unknown key $key for $name")
        }

      case "event"::head::Nil =>
        propertyDef(head, name, executionResult)
          .flatMap(_.data(getExecutions(identifier, executionResult)))
      case _ => Failure(s"Ethereum event only support one level of properties. invalid property access ${keys.mkString(".")}")
    }
  }

  private def getExecutions(identifier:ActionIdentifier, executionResult: TemplateExecutionResult):Seq[EthereumEventFilterExecution] = {
    executionResult.executions.get(identifier)
      .map(_.executionMap.values.toSeq.map(VariableType.convert[EthereumEventFilterExecution]))
      .getOrElse(Seq())
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
        varDefinitions.map(definition => definition.name -> definition)

      val types = varDefinitions.map(definition => definition.name -> definition.varType(executionResult))

      val structure = Structure(typeDefinition = typeDefinitions.toMap, types = types.toMap, names = typeDefinitions.map { case(k, _) => k })
      AbstractStructureType.generateType(name, structure)
    })
  }

  override def getTypeClass: Class[_ <: EventFilterDefinition] = classOf[EventFilterDefinition]

  def thisType: VariableType = EthereumEventFilterType

  override def actionValue(value: OpenlawValue): EventFilterDefinition = VariableType.convert[EventFilterDefinition](value)
}
