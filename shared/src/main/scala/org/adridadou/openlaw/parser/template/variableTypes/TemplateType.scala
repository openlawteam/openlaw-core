package org.adridadou.openlaw.parser.template.variableTypes

import cats.Eq
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.values._

import scala.util.Try

case class TemplateDefinition(name:TemplateSourceIdentifier, mappingInternal:Map[String, Expression] = Map(), path:Option[TemplatePath] = None) {
  lazy val mapping: Map[VariableName, Expression] = mappingInternal.map({case (key,value) => VariableName(key) -> value})
}
case class TemplateSourceIdentifier(name:TemplateTitle)

case class TemplatePath(path:Seq[String] = Seq()) {
  def innerFile(name:String):TemplatePath = TemplatePath(path ++ Seq(name))
}

case object TemplatePathType extends VariableType("TemplateType") with NoShowInForm {
  override def cast(value: String, executionResult: TemplateExecutionResult): TemplatePath =
    decode[TemplatePath](value) match {
      case Right(result) =>
        result
      case Left(ex) =>
        throw new RuntimeException(ex)
    }

  override def getTypeClass: Class[_ <: TemplatePathType.type ] = this.getClass

  override def internalFormat(value: Any): String =
    VariableType.convert[TemplatePath](value).asJson.noSpaces

  override def thisType: VariableType =
    TemplatePathType

  override def divide(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Any] = {
    for {
      left <- optLeft.map(VariableType.convert[TemplatePath])
      right <- optRight.map(VariableType.convert[String])
    } yield TemplatePath(left.path ++ Seq(right))
  }
}

object TemplateDefinition {
  implicit val templateDefinitionEnc: Encoder[TemplateDefinition] = deriveEncoder[TemplateDefinition]
  implicit val templateDefinitionDec: Decoder[TemplateDefinition] = deriveDecoder[TemplateDefinition]
  implicit val templateDefinitionEq:Eq[TemplateDefinition] = Eq.fromUniversalEquals

  implicit val templateIdentifierEnc: Encoder[TemplateSourceIdentifier] = deriveEncoder[TemplateSourceIdentifier]
  implicit val templateIdentifierDec: Decoder[TemplateSourceIdentifier] = deriveDecoder[TemplateSourceIdentifier]
  implicit val templateIdentifierEq:Eq[TemplateSourceIdentifier] = Eq.fromUniversalEquals
}

object TemplatePath {
  implicit val templateIdentifierEnc: Encoder[TemplatePath] = deriveEncoder[TemplatePath]
  implicit val templateIdentifierDec: Decoder[TemplatePath] = deriveDecoder[TemplatePath]
  implicit val templateIdentifierEq:Eq[TemplatePath] = Eq.fromUniversalEquals
}

case object TemplateType extends VariableType("Template") with NoShowInForm {
  private val availableParameters = Seq("parameters", "name", "path")
  implicit val eqForTemplateCategoryType: Eq[TemplateKind] = Eq.fromUniversalEquals[TemplateKind]

  def apply(name:String):TemplateKind = name match {
    case Agreement.name => Agreement
    case Deal.name => Deal
    case _ => Unknown
  }

  private def prepareTemplateName(mappingParameter: Parameters, executionResult: TemplateExecutionResult):Either[Throwable, Option[TemplateSourceIdentifier]] = {

    val unknownParameters = mappingParameter.parameterMap.map({case (key,_) => key})
      .filter(elem => !availableParameters.contains(elem))

    if(unknownParameters.nonEmpty) {
      Left(new Exception(s"unknown parameters ${unknownParameters.mkString(",")}. only ${availableParameters.mkString(",")} are allowed"))
    } else {
      getMandatoryParameter("name", mappingParameter) match {
        case templateName: OneValueParameter =>
          Try(templateName.expr.evaluate(executionResult)
            .map(VariableType.convert[String])
            .map(title => TemplateSourceIdentifier(TemplateTitle(title)))
          ).toEither
        case _ =>
          Left(new Exception("parameter 'name' accepts only single value"))
      }
    }
  }

  private def prepareTemplatePath(parameters: Parameters, executionResult:TemplateExecutionResult):Either[Throwable, Option[TemplatePath]] = parameters.parameterMap.toMap.get("path") match {
    case Some(OneValueParameter(expr)) =>
      expr.evaluate(executionResult).map({
        case p: TemplatePath =>
          Right(Some(p))
        case p: String =>
          Right(Some(TemplatePath(Seq(p))))
        case other =>
          Left(new Exception(s"parameter 'path' should be a path but instead was ${other.getClass.getSimpleName}"))
      }).getOrElse(Right(None))
    case Some(other) =>
      Left(new Exception(s"parameter 'path' should be a single value, not ${other.getClass.getSimpleName}"))
    case None =>
      Right(None)
  }

  private def prepareTemplateMappingParamters(parameters: Parameters, result: TemplateExecutionResult):Either[Throwable, Map[VariableName, Expression]] = parameters.parameterMap.toMap.get("parameters").map({
    case mapping:MappingParameter =>
      Right(mapping.mapping)
    case _ =>
      Left(new Exception("parameter 'parameters' should be a list of mapping values"))
  }).getOrElse(Right(Map()))

  def prepareTemplateSource(mappingParameter: Parameters, executionResult: TemplateExecutionResult, parameters: Map[VariableName, Expression], path: Option[TemplatePath]):Either[Throwable, TemplateDefinition] = {
    prepareTemplateName(mappingParameter, executionResult).flatMap({
      case Some(source) =>
        Right(TemplateDefinition(name = source, path = path, mappingInternal = parameters.map({ case (key, value) => key.name -> value })))
      case None =>
        Left(new Exception("name cannot be resolved yet!"))
    })
  }

  override def construct(constructorParams:Parameter, executionResult: TemplateExecutionResult):Either[Throwable, Option[TemplateDefinition]] = {
    constructorParams match {
      case mappingParameter:Parameters =>
        for {
          path <- prepareTemplatePath(mappingParameter, executionResult)
          parameters <- prepareTemplateMappingParamters(mappingParameter, executionResult)
          source <- prepareTemplateSource(mappingParameter, executionResult, parameters, path)
        } yield Some(source)

      case _ =>
        super.construct(constructorParams, executionResult)
          .map({
            case Some(n) => Some(TemplateDefinition(TemplateSourceIdentifier(TemplateTitle(n.toString))))
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
          throw new RuntimeException(s"properties '${tail.mkString(".")}' could not be resolved in sub template '$head'")
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
                throw new RuntimeException(s"property '${tail.mkString(".")}' could not be resolved in sub template '$head'")
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
          Failure(s"property '${keys.mkString(".")}' could not be resolved in sub template '${name.name}'")
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

  override def getTypeClass: Class[_ <: TemplateType.type ] = this.getClass

  override def cast(value: String, executionResult: TemplateExecutionResult): TemplateDefinition = handleEither(decode[TemplateDefinition](value))
  override def internalFormat(value: Any): String = VariableType.convert[TemplateDefinition](value).asJson.noSpaces
  override def defaultFormatter: Formatter = new NoopFormatter

  def thisType: VariableType = TemplateType
}