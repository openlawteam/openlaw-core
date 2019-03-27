
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

case class ClauseDefinition(name:ClauseSourceIdentifier, mappingInternal:Map[String, Expression] = Map(), path:Option[ClausePath] = None) {
  lazy val mapping: Map[VariableName, Expression] = mappingInternal.map({case (key,value) => VariableName(key) -> value})
}

case class ClauseSourceIdentifier(name:TemplateTitle)

case class ClausePath(path:Seq[String] = Seq()) {
  def innerFile(name:String):ClausePath = ClausePath(path ++ Seq(name))
}

case object ClausePathType extends VariableType("ClauseType") with NoShowInForm {
  override def cast(value: String, executionResult: TemplateExecutionResult): ClausePath =
    decode[ClausePath](value) match {
      case Right(result) =>
        result
      case Left(ex) =>
        throw new RuntimeException(ex)
    }

  override def getTypeClass: Class[_ <: ClausePathType.type ] = this.getClass

  override def internalFormat(value: Any): String =
    VariableType.convert[ClausePath](value).asJson.noSpaces

  override def thisType: VariableType =
    ClausePathType

  override def divide(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Any] = {
    for {
      left <- optLeft.map(VariableType.convert[ClausePath])
      right <- optRight.map(VariableType.convert[String])
    } yield ClausePath(left.path ++ Seq(right))
  }
}


object ClauseDefinition {
  implicit val clauseDefinitionEnc: Encoder[ClauseDefinition] = deriveEncoder[ClauseDefinition]
  implicit val clauseDefinitionDec: Decoder[ClauseDefinition] = deriveDecoder[ClauseDefinition]
  implicit val clauseDefinitionEq:Eq[ClauseDefinition] = Eq.fromUniversalEquals

  implicit val clauseIdentifierEnc: Encoder[ClauseSourceIdentifier] = deriveEncoder[ClauseSourceIdentifier]
  implicit val clauseIdentifierDec: Decoder[ClauseSourceIdentifier] = deriveDecoder[ClauseSourceIdentifier]
  implicit val clauseIdentifierEq:Eq[ClauseSourceIdentifier] = Eq.fromUniversalEquals
}

object ClausePath {
  implicit val clausePathEnc: Encoder[ClausePath] = deriveEncoder[ClausePath]
  implicit val clausePathDec: Decoder[ClausePath] = deriveDecoder[ClausePath]
  implicit val clausePathEq:Eq[ClausePath] = Eq.fromUniversalEquals
}

case object ClauseType extends VariableType("Clause") with NoShowInForm {
  private val availableParameters = Seq("parameters", "name", "path")

  private def prepareClauseName(mappingParameter: Parameters, executionResult: TemplateExecutionResult):Result[Option[ClauseSourceIdentifier]] = {

    val unknownParameters = mappingParameter.parameterMap.map({case (key,_) => key})
      .filter(elem => !availableParameters.contains(elem))

    if(unknownParameters.nonEmpty) {
      Failure(s"unknown parameters ${unknownParameters.mkString(",")}. only ${availableParameters.mkString(",")} are allowed")
    } else {
      getMandatoryParameter("name", mappingParameter) match {
        case templateName: OneValueParameter =>
          attempt(templateName.expr.evaluate(executionResult)
            .map(VariableType.convert[String])
            .map(title => ClauseSourceIdentifier(TemplateTitle(title))))
        case _ =>
          Failure("parameter 'name' accepts only single value")
      }
    }
  }

  private def prepareClausePath(parameters: Parameters, executionResult:TemplateExecutionResult):Result[Option[ClausePath]] = parameters.parameterMap.toMap.get("path") match {
    case Some(OneValueParameter(expr)) =>
      expr.evaluate(executionResult).map({
        case p: ClausePath =>
          Success(Some(p))
        case p: String =>
          Success(Some(ClausePath(Seq(p))))
        case other =>
          Failure(s"parameter 'path' should be a path but instead was ${other.getClass.getSimpleName}")
      }).getOrElse(Right(None))
    case Some(other) =>
      Failure(s"parameter 'path' should be a single value, not ${other.getClass.getSimpleName}")
    case None =>
      Success(None)
  }

  //may be needed for mapping so we don't have to make unneeded methods in execution engine?

  def convertToTemplateSourceIdentifier(identifier:ClauseSourceIdentifier): TemplateSourceIdentifier = {
    TemplateSourceIdentifier(identifier.name)
  }

  private def prepareClauseMappingParamters(parameters: Parameters, result: TemplateExecutionResult):Result[Map[VariableName, Expression]] = parameters.parameterMap.toMap.get("parameters").map({
    case mapping:MappingParameter =>
      Success(mapping.mapping)
    case _ =>
      Failure("parameter 'parameters' should be a list of mapping values")
  }).getOrElse(Success(Map()))

  def prepareClauseSource(mappingParameter: Parameters, executionResult: TemplateExecutionResult, parameters: Map[VariableName, Expression], path: Option[ClausePath]):Result[ClauseDefinition] = {
    prepareClauseName(mappingParameter, executionResult).flatMap({
      case Some(source) =>
        Success(ClauseDefinition(name = source, path = path, mappingInternal = parameters.map({ case (key, value) => key.name -> value })))
      case None =>
        Failure("name cannot be resolved yet!")
    })
  }

  override def construct(constructorParams:Parameter, executionResult: TemplateExecutionResult):Result[Option[ClauseDefinition]] = {
    constructorParams match {
      case mappingParameter:Parameters =>
        for {
          path <- prepareClausePath(mappingParameter, executionResult)
          parameters <- prepareClauseMappingParamters(mappingParameter, executionResult)
          source <- prepareClauseSource(mappingParameter, executionResult, parameters, path)
        } yield Some(source)

      case _ =>
        super.construct(constructorParams, executionResult)
          .map({
            case Some(n) => Some(ClauseDefinition(ClauseSourceIdentifier(TemplateTitle(n.toString))))
            case None => None
          })
    }
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
  }

  override def getTypeClass: Class[_ <: ClauseType.type ] = this.getClass

  override def cast(value: String, executionResult: TemplateExecutionResult): ClauseDefinition = handleEither(decode[ClauseDefinition](value))
  override def internalFormat(value: Any): String = VariableType.convert[ClauseDefinition](value).asJson.noSpaces
  override def defaultFormatter: Formatter = new NoopFormatter

  def thisType: VariableType = ClauseType
}