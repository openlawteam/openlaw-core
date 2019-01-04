package org.adridadou.openlaw.parser.template.variableTypes

import cats.Eq
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.values._

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

  override def construct(constructorParams:Parameter, executionResult: TemplateExecutionResult):Option[TemplateDefinition] = {
    constructorParams match {
      case mappingParameter:Parameters =>
        val unknownParameters = mappingParameter.parameterMap.map({case (key,_) => key})
          .filter(elem => !availableParameters.contains(elem))

        if(unknownParameters.nonEmpty) {
          throw new RuntimeException(s"unknown parameters ${unknownParameters.mkString(",")}. only ${availableParameters.mkString(",")} are allowed")
        }

        val name = getMandatoryParameter("name", mappingParameter) match {
          case templateName: OneValueParameter =>
            templateName.expr.evaluate(executionResult)
              .map(VariableType.convert[String]).map(title => TemplateSourceIdentifier(TemplateTitle(title)))
          case _ =>
            throw new RuntimeException("parameter 'name' accepts only single value")
        }

        val path = mappingParameter.parameterMap.toMap.get("path").flatMap({
          case OneValueParameter(expr) =>
            expr.evaluate(executionResult).map({
              case p:TemplatePath =>
                p
              case p:String =>
                TemplatePath(Seq(p))
              case other =>
                throw new RuntimeException(s"parameter 'path' should be a path but instead was ${other.getClass.getSimpleName}")
            })
          case _ =>
            throw new RuntimeException("parameter 'path' should be a single value")
        })

        val parameters = mappingParameter.parameterMap.toMap.get("parameters").map({
          case mapping:MappingParameter =>
            mapping.mapping
          case _ =>
            throw new RuntimeException("parameter 'parameters' should be a list of mapping values")
        }).getOrElse(Map())

        name match {
          case Some(source) =>
            Some(TemplateDefinition(name = source, path = path, mappingInternal = parameters.map({case (key,value) => key.name -> value})))
          case None =>
            throw new RuntimeException("name cannot be resolved yet!")
        }

      case _ =>
        super.construct(constructorParams, executionResult)
          .map(name => TemplateDefinition(TemplateSourceIdentifier(TemplateTitle(name.toString))))
    }
  }

  override def access(value: Any, keys: Seq[String], executionResult: TemplateExecutionResult): Either[String, Any] = keys.toList match {
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

  override def validateKeys(name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Option[String] = keys.toList match {
    case Nil =>
      None
    case head::tail =>
      executionResult.subExecutions.get(name).flatMap(subExecution =>
        subExecution.getExpression(VariableName(head))
          .map(variable => variable.expressionType(subExecution).keysType(tail,subExecution))) match {
        case Some(_) =>
          None
        case None =>
          Some(s"property '${keys.mkString(".")}' could not be resolved in sub template '${name.name}'")
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