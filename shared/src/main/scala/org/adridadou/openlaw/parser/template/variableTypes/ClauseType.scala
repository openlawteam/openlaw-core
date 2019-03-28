
package org.adridadou.openlaw.parser.template.variableTypes

import cats.Eq
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.{MappingParameter, OneValueParameter, Parameter, Parameters, TemplateExecutionResult, VariableName}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import org.adridadou.openlaw.values._

case object ClauseType extends VariableType("Clause") with NoShowInForm {
  private val availableParameters = Seq("parameters", "name")

  /*private def prepareClauseName(mappingParameter: Parameters, executionResult: TemplateExecutionResult):Result[Option[TemplateSourceIdentifier]] = {

    val unknownParameters = mappingParameter.parameterMap.map({case (key,_) => key})
      .filter(elem => !availableParameters.contains(elem))

    if(unknownParameters.nonEmpty) {
      Failure(s"unknown parameters ${unknownParameters.mkString(",")}. only ${availableParameters.mkString(",")} are allowed")
    } else {
      getMandatoryParameter("name", mappingParameter) match {
        case templateName: OneValueParameter =>
          attempt(templateName.expr.evaluate(executionResult)
            .map(VariableType.convert[String])
            .map(title => TemplateSourceIdentifier(TemplateTitle(title))))
        case _ =>
          Failure("parameter 'name' accepts only single value")
      }
    }
  }

  private def prepareClauseMappingParamters(parameters: Parameters, result: TemplateExecutionResult):Result[Map[VariableName, Expression]] = parameters.parameterMap.toMap.get("parameters").map({
    case mapping:MappingParameter =>
      Success(mapping.mapping)
    case _ =>
      Failure("parameter 'parameters' should be a list of mapping values")
  }).getOrElse(Success(Map()))

  def prepareClauseSource(mappingParameter: Parameters, executionResult: TemplateExecutionResult, parameters: Map[VariableName, Expression], path: Option[TemplatePath]):Result[TemplateDefinition] = {
    prepareClauseName(mappingParameter, executionResult).flatMap({
      case Some(source) =>
        Success(TemplateDefinition(name = source, path = path, mappingInternal = parameters.map({ case (key, value) => key.name -> value })))
      case None =>
        Failure("name cannot be resolved yet!")
    })
  }

  override def access(value: Any, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Any] = keys.toList match {
    case Nil =>
      Right(value)
    case head::tail =>
      executionResult.subExecutions.get(VariableName(head)).flatMap(subExecution =>
        subExecution.getExpression(VariableName(head))
          .flatMap(variable => variable.evaluate(subExecution)
            .map(subValue => variable.expressionType(subExecution).access(subValue, tail, subExecution))
          )) match {
        case Some(result) =>
          result
        case None =>
          throw new RuntimeException(s"properties '${tail.mkString(".")}' could not be resolved in sub clause '$head'")
      }
  }

  override def keysType(keys: Seq[String], executionResult: TemplateExecutionResult): VariableType = {
    keys.toList match {
      case Nil =>
        TemplateType
      case head::tail =>
        executionResult.subExecutions.get(VariableName(head)).flatMap(subExecution =>
          subExecution.getExpression(VariableName(head))
            .map(_.expressionType(subExecution).keysType(tail, subExecution))) match {
          case Some(varType) =>
            varType
          case None =>
            throw new RuntimeException(s"property '${tail.mkString(".")}' could not be resolved in sub clause '$head'")
        }
    }
  }

  override def validateKeys(name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Unit] = keys.toList match {
    case Nil =>
      Success(())
    case head::tail =>
      executionResult.subExecutions.get(name).flatMap(subExecution =>
        subExecution.getExpression(VariableName(head))
          .map(variable => variable.expressionType(subExecution).keysType(tail,subExecution))) match {
        case Some(_) =>
          Success(())
        case None =>
          Failure(s"property '${keys.mkString(".")}' could not be resolved in sub clause '${name.name}'")
      }
  }

  override def accessVariables(name:VariableName, keys:Seq[String], executionResult: TemplateExecutionResult): Seq[VariableName] = keys.toList match {
    case Nil =>
      Seq()
    case head::tail =>
      executionResult.subExecutions.get(name).flatMap(subExecution =>
        subExecution.getExpression(VariableName(head))
          .map(variable => variable
            .expressionType(subExecution)
            .accessVariables(VariableName(head), tail, subExecution)
          )
      ).getOrElse(Seq(name))
  }*/

  override def getTypeClass: Class[_ <: ClauseType.type ] = this.getClass

  override def cast(value: String, executionResult: TemplateExecutionResult): TemplateDefinition = handleEither(decode[TemplateDefinition](value))
  override def internalFormat(value: Any): String = VariableType.convert[TemplateDefinition](value).asJson.noSpaces
  override def defaultFormatter: Formatter = new NoopFormatter

  def thisType: VariableType = ClauseType
}