package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import cats.Eq
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.{
  Formatter,
  NoopFormatter
}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}
import org.adridadou.openlaw.values._

final case class TemplateDefinition(
    name: TemplateSourceIdentifier,
    mapping: Map[VariableName, Expression] = Map.empty,
    path: Option[TemplatePath] = None
) extends OpenlawNativeValue

final case class TemplateSourceIdentifier(name: TemplateTitle)

final case class TemplatePath(path: List[String] = Nil)
    extends OpenlawNativeValue {
  def innerFile(name: String): TemplatePath = TemplatePath(path ++ List(name))
}

case object TemplatePathType
    extends VariableType("TemplateType")
    with NoShowInForm {
  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[TemplatePath] =
    decode[TemplatePath](value).leftFlatMap {
      case e: Exception => Failure(e)
      case e =>
        throw e // We only want to handle non-fatal exceptions, so we throw any Errors
    }

  override def getTypeClass: Class[TemplateDefinition] =
    classOf[TemplateDefinition]

  override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType.convert[TemplatePath](value).map(_.asJson.noSpaces)

  override def thisType: VariableType =
    TemplatePathType

  override def divide(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[TemplatePath]] =
    TemplatePathType.divide(left, right, executionResult)
}

object TemplateDefinition {
  implicit val templateDefinitionEnc: Encoder[TemplateDefinition] =
    deriveEncoder
  implicit val templateDefinitionDec: Decoder[TemplateDefinition] =
    deriveDecoder
  implicit val templateDefinitionEq: Eq[TemplateDefinition] =
    Eq.fromUniversalEquals
}

object TemplateSourceIdentifier {
  implicit val templateIdentifierEnc: Encoder[TemplateSourceIdentifier] =
    deriveEncoder
  implicit val templateIdentifierDec: Decoder[TemplateSourceIdentifier] =
    deriveDecoder
  implicit val templateIdentifierEq: Eq[TemplateSourceIdentifier] =
    Eq.fromUniversalEquals
}

object TemplatePath {
  implicit val templatePathEnc: Encoder[TemplatePath] = deriveEncoder
  implicit val templatePathDec: Decoder[TemplatePath] = deriveDecoder
  implicit val templatePathEq: Eq[TemplatePath] = Eq.fromUniversalEquals
}

case object TemplateType extends VariableType("Template") with NoShowInForm {
  private val availableParameters = Seq("parameters", "name", "path")

  private def prepareTemplateName(
      mappingParameter: Parameters,
      executionResult: TemplateExecutionResult
  ): Result[Option[TemplateSourceIdentifier]] = {

    val unknownParameters = mappingParameter.parameterMap
      .map({ case (key, _) => key.trim })
      .filter(elem => !availableParameters.contains(elem))

    if (unknownParameters.nonEmpty) {
      Failure(s"unknown parameters ${unknownParameters
        .mkString(",")}. only ${availableParameters.mkString(",")} are allowed")
    } else {
      getMandatoryParameter("name", mappingParameter).flatMap {
        case templateName: OneValueParameter =>
          templateName.expr
            .evaluate(executionResult)
            .flatMap(_.map(VariableType.convert[OpenlawString]).sequence)
            .map(
              _.map(title => TemplateSourceIdentifier(TemplateTitle(title)))
            )
        case _ =>
          Failure("parameter 'name' accepts only single value")
      }
    }
  }

  private def prepareTemplatePath(
      parameters: Parameters,
      executionResult: TemplateExecutionResult
  ): Result[Option[TemplatePath]] =
    parameters.parameterMap.toMap.get("path") match {
      case Some(OneValueParameter(expr)) =>
        expr.evaluate(executionResult).flatMap { option =>
          option
            .map({
              case p: TemplatePath =>
                Success(Some(p))
              case p: OpenlawString =>
                Success(Some(TemplatePath(List(p.underlying))))
              case other =>
                Failure(
                  s"parameter 'path' should be a path but instead was ${other.getClass.getSimpleName}"
                )
            })
            .getOrElse(Success(None))
        }
      case Some(other) =>
        Failure(
          s"parameter 'path' should be a single value, not ${other.getClass.getSimpleName}"
        )
      case None =>
        Success(None)
    }

  private def prepareTemplateMappingParameters(
      parameters: Parameters,
      result: TemplateExecutionResult
  ): Result[Map[VariableName, Expression]] =
    parameters.parameterMap.toMap
      .get("parameters")
      .map({
        case mapping: MappingParameter =>
          Success(mapping.mapping)
        case _ =>
          Failure("parameter 'parameters' should be a list of mapping values")
      })
      .getOrElse(Success(Map.empty))

  def prepareTemplateSource(
      mappingParameter: Parameters,
      executionResult: TemplateExecutionResult,
      parameters: Map[VariableName, Expression],
      path: Option[TemplatePath]
  ): Result[TemplateDefinition] = {
    prepareTemplateName(mappingParameter, executionResult).flatMap({
      case Some(source) =>
        Success(
          TemplateDefinition(name = source, path = path, mapping = parameters)
        )
      case None =>
        Failure("name cannot be resolved yet!")
    })
  }

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[TemplateDefinition]] =
    constructorParams match {
      case mappingParameter: Parameters =>
        for {
          path <- prepareTemplatePath(mappingParameter, executionResult)
          parameters <- prepareTemplateMappingParameters(
            mappingParameter,
            executionResult
          )
          source <- prepareTemplateSource(
            mappingParameter,
            executionResult,
            parameters,
            path
          )
        } yield Some(source)

      case _ =>
        super
          .construct(constructorParams, executionResult)
          .map({
            case Some(n) =>
              Some(
                TemplateDefinition(
                  TemplateSourceIdentifier(TemplateTitle(n.toString))
                )
              )
            case None => None
          })
    }

  override def access(
      value: OpenlawValue,
      name: VariableName,
      keys: List[VariableMemberKey],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] = keys match {
    case Nil =>
      Success(Some(value))
    case head :: tail =>
      head.key match {
        case Left(variableName) =>
          executionResult.subExecutions
            .get(name)
            .flatMap { subExecution =>
              subExecution
                .getExpression(variableName)
                .flatMap { variable =>
                  variable
                    .evaluate(subExecution)
                    .map { option =>
                      option.map { subValue =>
                        variable
                          .expressionType(subExecution)
                          .flatMap(
                            _.access(subValue, variableName, tail, subExecution)
                          )
                      }
                    }
                    .sequence
                    .map(_.flatten)
                }
            }
            .getOrElse(
              Failure(
                s"properties '${keys.mkString(".")}' could not be resolved in sub template '$name'"
              )
            )
        case _ => Failure("template property are never a function")
      }
  }

  override def keysType(
      keys: List[VariableMemberKey],
      expr: Expression,
      executionResult: TemplateExecutionResult
  ): Result[VariableType] =
    keys match {
      case Nil => Success(ClauseType)
      case head :: tail =>
        for {
          variableName <- expr match {
            case v: VariableName => Success(v)
            case other =>
              Failure(
                "property access only works with variable names (no expressions)"
              )
          }
          result <- head.key match {
            case Left(firstVariable) =>
              executionResult.subExecutions.get(variableName).flatMap {
                subExecution =>
                  subExecution
                    .getExpression(firstVariable)
                    .map(subExpr =>
                      subExpr
                        .expressionType(subExecution)
                        .flatMap(_.keysType(tail, subExpr, subExecution))
                    )
              } match {
                case Some(varType) => varType
                case None =>
                  Failure(
                    s"property '${keys.mkString(".")}' could not be resolved in sub template '$expr'"
                  )
              }
            case _ => Failure("template property are never a function")
          }
        } yield result
    }

  override def validateKeys(
      name: VariableName,
      keys: List[VariableMemberKey],
      expr: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] = keys match {
    case Nil =>
      Success.unit
    case head :: tail =>
      head.key match {
        case Left(variableName) =>
          executionResult.subExecutions.get(name).flatMap { subExecution =>
            subExecution
              .getExpression(variableName)
              .map(variable =>
                variable
                  .expressionType(subExecution)
                  .flatMap(_.keysType(tail, variable, subExecution))
              )
          } match {
            case Some(_) =>
              Success.unit
            case None =>
              Failure(
                s"property '${keys.mkString(".")}' could not be resolved in sub template '${name.name}'"
              )
          }
        case _ => Failure("template property are never a function")
      }
  }

  override def accessVariables(
      name: VariableName,
      keys: List[VariableMemberKey],
      executionResult: TemplateExecutionResult
  ): Result[List[VariableName]] = keys match {
    case Nil =>
      Success(Nil)
    case head :: tail =>
      head.key match {
        case Left(variableName) =>
          executionResult.subExecutions
            .get(name)
            .flatMap { subExecution =>
              subExecution
                .getExpression(variableName)
                .map(variable =>
                  variable
                    .expressionType(subExecution)
                    .flatMap(
                      _.accessVariables(variableName, tail, subExecution)
                    )
                )
            }
            .getOrElse(Success(List(name)))
        case _ => Failure("template property are never a function")
      }
  }

  override def getTypeClass: Class[_ <: TemplateDefinition] =
    classOf[TemplateDefinition]

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[TemplateDefinition] =
    decode[TemplateDefinition](value).leftMap(FailureException(_))
  override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType.convert[TemplateDefinition](value).map(_.asJson.noSpaces)
  override def defaultFormatter: Formatter = new NoopFormatter

  def thisType: VariableType = TemplateType
}
