package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.parser._
import io.circe.syntax._
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case object ExternalCallType extends VariableType("ExternalCall") with ActionType {

  case class PropertyDef(typeDef: VariableType, data: Seq[ExternalCallExecution] => Option[OpenlawValue])

  private val propertyDef: Map[String, PropertyDef] = Map[String, PropertyDef](
    "status" -> PropertyDef(typeDef = TextType, _.headOption.map(_.executionStatus.name)),
    "executionDate" -> PropertyDef(typeDef = DateTimeType, _.headOption.map(_.executionDate)))

  override def cast(value: String, executionResult: TemplateExecutionResult): ExternalCall =
    handleEither(decode[ExternalCall](value))

  override def internalFormat(value: OpenlawValue): String = value match {
    case call: ExternalCall =>
      call.asJson.noSpaces
  }

  override def validateKeys(variableName: VariableName, keys: Seq[String], valueExpression: Expression, executionResult: TemplateExecutionResult): Result[Unit] = {
    keys.toList match {
      case Nil => Success.unit
      case prop :: Nil => propertyDef.get(prop) match {
        case Some(_) =>
          Success.unit
        case None if "result".equalsIgnoreCase(prop) =>
          Success.unit
        case None =>
          Failure(s"unknown property $prop for ExternalExecution type")
      }
      case prop :: otherKeys if prop.equalsIgnoreCase("result") =>
        valueExpression.evaluateT[ExternalCall](executionResult).flatMap(getIntegratedService(_, executionResult)) match {
          case Some(integratedServiceDefinition) =>
            val variableType = AbstractStructureType.generateType(variableName, integratedServiceDefinition.output)
            variableType.validateKeys(variableName, otherKeys, valueExpression, executionResult)
          case None =>
            Failure(s"unable to get ${keys.mkString(".")} missing value or service definition")
        }
      case _ =>
        Failure(s"unknown property ${keys.mkString(".")} for ExternalExecution type")
    }
  }

  override def keysType(keys: Seq[String], valueExpression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] = {
    keys.toList match {
      case Nil => Success(this)
      case prop :: Nil => propertyDef.get(prop) match {
        case Some(propDef) =>
          Success(propDef.typeDef)
        case None if "result".equalsIgnoreCase(prop) =>
          Success(this)
        case None =>
          Failure(s"unknown property $prop for ExternalExecution type")
      }
      case prop :: otherKeys if prop.equalsIgnoreCase("result") =>
        valueExpression.evaluateT[ExternalCall](executionResult).flatMap(getIntegratedService(_, executionResult)) match {
          case Some(integratedServiceDefinition) =>
            val variableType = AbstractStructureType.generateType(VariableName("Output"), integratedServiceDefinition.output)
            variableType.keysType(otherKeys, valueExpression, executionResult)
          case None =>
            Failure(s"unable to get ${keys.mkString(".")} missing value or service definition")
        }
      case _ =>
        Failure(s"unknown property ${keys.mkString(".")} for ExternalExecution type")
    }
  }

  override def access(value: OpenlawValue, name: VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    val actionIdentifier = VariableType.convert[ExternalCall](value).identifier(executionResult)
    val executions = executionResult.executions.get(actionIdentifier).map(_.executionMap.values.toSeq)
      .getOrElse(Seq())
      .map(VariableType.convert[ExternalCallExecution])

    keys.toList match {
      case Nil => Success(Some(value))
      case prop :: Nil => propertyDef.get(prop) match {
        case Some(propDef) =>
          Success(propDef.data(executions))
        case None if "result".equalsIgnoreCase(prop) =>

          getIntegratedService(VariableType.convert[ExternalCall](value), executionResult) match {
            case Some(integratedServiceDefinition) =>
              val variableType = AbstractStructureType.generateType(name, integratedServiceDefinition.output)
              attempt(executions.flatMap({
                case e:SuccessfulExternalCallExecution => Some(e)
                case _ => None
              }).map(_.result)
                .map(variableType.cast(_, executionResult))
                .headOption)
            case None =>
              Failure(s"unable to get ${keys.mkString(".")} missing value or service definition")
          }
        case None =>
          Failure(s"unknown property $prop for ExternalExecution type")
      }
      case prop :: otherKeys if "result".equalsIgnoreCase(prop) =>
        getIntegratedService(VariableType.convert[ExternalCall](value), executionResult) match {
          case Some(integratedServiceDefinition) =>
            val variableType = AbstractStructureType.generateType(name, integratedServiceDefinition.output)
            executions.flatMap({
              case e:SuccessfulExternalCallExecution => Some(e)
              case _ => None
            }).map(_.result)
              .map(variableType.cast(_, executionResult))
              .map(s => variableType.access(s, name, otherKeys, executionResult))
              .headOption
              .getOrElse(Success(None))
          case None =>
            Failure(s"unable to get ${keys.mkString(".")} missing value or service definition")
        }
      case _ =>
        Failure(s"unknown property ${keys.mkString(".")} for ExternalExecution type")
    }
  }

  private def getIntegratedService(externalCall: ExternalCall, executionResult: TemplateExecutionResult):Option[IntegratedServiceDefinition] =
    externalCall.serviceName
      .evaluateT[OpenlawString](executionResult)
      .map(ServiceName(_))
      .flatMap(executionResult.externalCallStructures.get)

  override def defaultFormatter: Formatter = new NoopFormatter

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[ExternalCall]] = {
    constructorParams match {
      case Parameters(v) =>
        val values = v.toMap
        attempt(Some(ExternalCall(
          serviceName = getExpression(values, "serviceName", "service", "name"),
          parameters = getParameter(values, "parameters", "params", "arguments", "args").map(prepareMappingParameters(_, executionResult)).getOrElse(Map()),
          startDate = values.get("startDate").map(name => getExpression(name)),
          endDate = getParameter(values, "endDate").map(getExpression),
          every = getParameter(values, "repeatEvery").map(getExpression)
        )))
      case _ =>
        Failure("ExternalCall needs to get 'serviceName' and 'arguments' as constructor parameters")
    }
  }

  private def prepareMappingParameters(parameter: Parameter, result: TemplateExecutionResult):Map[VariableName, Expression] = parameter match {
    case mapping:MappingParameter =>
      mapping.mapping
    case _ =>
      throw new RuntimeException(s"parameter 'parameters' should be a list of mapping values not ${parameter.getClass.getSimpleName}")
  }

  override def getTypeClass: Class[_ <: ExternalCall] = classOf[ExternalCall]

  def thisType: VariableType = ExternalCallType

  override def actionValue(value: OpenlawValue): ExternalCall = VariableType.convert[ExternalCall](value)
}

