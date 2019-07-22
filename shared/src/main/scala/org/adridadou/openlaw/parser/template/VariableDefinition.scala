package org.adridadou.openlaw.parser.template

import cats.Eq
import cats.implicits._
import io.circe.{Decoder, Encoder, HCursor, Json, KeyDecoder, KeyEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, Result, Success}

import scala.reflect.ClassTag

case class VariableMember(name:VariableName, keys:Seq[String], formatter:Option[FormatterDefinition]) extends TemplatePart with Expression {
  override def missingInput(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] =
    name.missingInput(executionResult)

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] =
    name
      .expressionType(executionResult)
      .flatMap(_.validateKeys(name, keys, name, executionResult))

  override def expressionType(executionResult:TemplateExecutionResult): Result[VariableType] =
    name
      .aliasOrVariable(executionResult)
      .flatMap { expression =>
        expression
          .expressionType(executionResult)
          .map(_.keysType(keys, expression, executionResult))
          .flatten
      }

  override def evaluate(executionResult:TemplateExecutionResult): Result[Option[OpenlawValue]] =
    (for {
      expr <- name.aliasOrVariable(executionResult)
      exprType <- expr.expressionType(executionResult)
      optValue <- expr.evaluate(executionResult)
    } yield {
      optValue
        .map(exprType.access(_, name, keys, executionResult))
        .sequence
        .map(_.flatten)
    }).flatten

  override def variables(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] =
    name.variables(executionResult)

  override def toString: String =
    (Seq(name.name) ++ keys).mkString(".")
}

case class VariableName(name:String) extends Expression {
  def isAnonymous: Boolean = name.trim === "_"

  override def validate(executionResult:TemplateExecutionResult): Result[Unit] = Success.unit

  override def expressionType(executionResult:TemplateExecutionResult): Result[VariableType] = {
    executionResult.getVariable(name) match {
      case Some(variable) =>
        Success(variable.varType(executionResult))
      case None =>
        executionResult.getAlias(name).map(_.expressionType(executionResult)).sequence.map(_.getOrElse(TextType))
    }
  }

  override def evaluate(executionResult:TemplateExecutionResult): Result[Option[OpenlawValue]] =
    executionResult.getVariable(name) match {
      case Some(variable) =>
        variable.evaluate(executionResult)
      case None =>
        executionResult
          .getAlias(name)
          .map(_.evaluate(executionResult))
          .sequence
          .map(_.flatten)
    }

  override def variables(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] =
    executionResult.getExpression(this) match {
      case Some(variable:VariableDefinition) =>
        variable
          .defaultValue
          .map(_.variables(executionResult))
          .sequence
          .map(_.getOrElse(Seq()))
          .map(Seq(this) ++ _)
      case Some(alias:VariableAliasing) =>
        alias.variables(executionResult)
      case Some(expression:Expression) =>
        expression.variables(executionResult)
      case None =>
        Success(Seq(this))
    }

  override def toString:String = name

  override def missingInput(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] = executionResult.getAliasOrVariableType(this) map {
    case _:NoShowInForm => Seq()
    case _ =>
      executionResult.getParameter(name) match {
        case Some(_) => Seq()
        case None => executionResult.getAlias(name) match {
          case Some(_) => Seq()
          case None => Seq(this)
        }
      }
  }

  def aliasOrVariable(executionResult:TemplateExecutionResult): Result[Expression] =
    executionResult.getAlias(name) match {
      case Some(expr) => Success(expr)
      case None => executionResult.getVariable(name) match {
        case Some(variable) => Success(variable)
        case None =>
          Failure(s"no alias or variable found with the name $name")
      }
    }

}

object FormatterDefinition {
  implicit val formatterDefinitionEnc:Encoder[FormatterDefinition] = deriveEncoder[FormatterDefinition]
  implicit val formatterDefinitionDec:Decoder[FormatterDefinition] = deriveDecoder[FormatterDefinition]
}

case class FormatterDefinition(name:String, parameters:Option[Parameter])

object VariableDefinition {
  def apply(name:String):VariableDefinition =
    VariableDefinition(name = VariableName(name))

  def apply(name:VariableName):VariableDefinition =
    new VariableDefinition(name = name)

  def apply(name:String, varType:VariableType):VariableDefinition =
    VariableDefinition(name = VariableName(name), Some(VariableTypeDefinition(varType.name)))

  implicit val variableDefinitionEnc: Encoder[VariableDefinition] = deriveEncoder[VariableDefinition]
  implicit val variableDefinitionDec: Decoder[VariableDefinition] = deriveDecoder[VariableDefinition]
}

object VariableName {
  implicit val variableNameEnc: Encoder[VariableName] = (a: VariableName) => Json.fromString(a.name)
  implicit val variableNameDec: Decoder[VariableName] = (c: HCursor) => c.as[String].map(n => VariableName(n))
  implicit val variableNameKeyEnc: KeyEncoder[VariableName] = (key: VariableName) => key.name
  implicit val variableNameKeyDec: KeyDecoder[VariableName] = (key: String) => Some(VariableName(key))
  implicit val variableNameEq:Eq[VariableName] = Eq.fromUniversalEquals

  def apply(name:String):VariableName = new VariableName(name.trim)
}

case class VariableDefinition(name: VariableName, variableTypeDefinition:Option[VariableTypeDefinition] = None, description:Option[String] = None, formatter:Option[FormatterDefinition] = None, isHidden:Boolean = false, defaultValue:Option[Parameter] = None) extends TextElement("VariableDefinition") with TemplatePart with Expression {

  def constructT[U <: OpenlawValue](executionResult: TemplateExecutionResult)(implicit classTag:ClassTag[U]): Result[Option[U#T]] = {
    construct(executionResult).flatMap({
      case Some(value) => VariableType.convert[U](value).map(Some(_))
      case None => Success(None)
    })
  }

  def construct(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = defaultValue match {
    case Some(parameter) =>
      varType(executionResult).construct(parameter, executionResult)
    case None if varType(executionResult) === OLOwnType => Success(Some(executionResult.info))
    case None => Success(None)
  }

  def isAnonymous: Boolean = name.isAnonymous

  def varType(executionResult: TemplateExecutionResult):VariableType = variableTypeDefinition
    .flatMap(typeDefinition => executionResult.findVariableType(typeDefinition)).getOrElse(TextType)

  def verifyConstructor(executionResult: TemplateExecutionResult): Result[Option[Any]] = {
    implicit val eqCls:Eq[Class[_]] = Eq.fromUniversalEquals
    defaultValue match {
      case Some(parameter) =>
        val variableType = varType(executionResult)

        variableType.construct(parameter, executionResult).flatMap({
          case Some(result) =>
            val expectedType = this.varType(executionResult).getTypeClass
            val resultType = result.getClass
            if(expectedType == resultType) {
              Success(Some(result))
            } else {
            Failure(s"type mismatch while building the default value for type ${variableType.name}. the constructor result type should be ${expectedType.getSimpleName} but instead is ${result.getClass.getSimpleName}")
          }
          case None => Success(None)
        })
      case None => Right(None)
    }
  }

  def cast(value: String, executionResult:TemplateExecutionResult): Result[OpenlawValue] =
    varType(executionResult).cast(value, executionResult)

  def nameOnly:Boolean = variableTypeDefinition === None

  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    executionResult.getAlias(this.name) match {
      case Some(alias) =>
        alias.evaluate(executionResult)
      case None =>
        val optVariable = executionResult.getVariable(this.name)
        executionResult.getAliasOrVariableType(this.name).flatMap {
          case _:NoShowInForm =>
            constructVariable(optVariable, executionResult)
          case _ =>
            executionResult.getParameter(this.name) match {
              case Some(value) =>
                optVariable.map(_.cast(value, executionResult)).sequence
              case None =>
                constructVariable(optVariable, executionResult)
            }
        }
    }
  }

  private def constructVariable(optVariable:Option[VariableDefinition], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    optVariable
      .map(_.construct(executionResult))
      .sequence
      .map(_.flatten)

  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] =
    executionResult
      .getAlias(name)
      .map(_.expressionType(executionResult))
      .getOrElse(executionResult.getVariable(name).map(_.varType(executionResult)) match {
        case Some(vt) =>
          Success(vt)
        case None =>
          Failure("the variable or alias '" + name + "' has not been defined. Please define it before using it in an expression")
      })

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] = {
    defaultValue.collect {
      case Parameters(parameterMap) =>
        parameterMap.toMap.get("options").map {
          case OneValueParameter(expr) =>
            validateOptions(Seq(expr), executionResult)
          case ListParameter(exprs) =>
            validateOptions(exprs, executionResult)
          case _ =>
            Success(())
        }
    }.flatten.getOrElse(Success(()))
  }

  private def validateOptions(exprs:Seq[Expression], executionResult: TemplateExecutionResult): Result[Unit] = {
    val currentType = varType(executionResult)

    currentType match {
      case choiceType:DefinedChoiceType =>
        // may use ResultNel to accumulate this errors
        exprs.map(validateOption(_, choiceType, executionResult)).headOption.getOrElse(Success(()))
      case _ =>
        exprs
          .flatMap {
            case expr@StringConstant(value, _) =>
              currentType.cast(value, executionResult) match {
                case Success(_) => None
                case Failure(_, _) => Some(expr)
              }
            case expr => Some(expr)
          }
          .map { expr => expr.expressionType(executionResult).map(x => expr -> x) }
          .toList
          .sequence
          .flatMap { list =>
            list
              .find { case (_, variableType) => variableType =!= currentType }
              .map { case (expr, variableType) => Failure(s"options element error! should be of type ${varType(executionResult).name} but ${expr.toString} is ${variableType.name} instead") }
              .getOrElse(Success(()))
          }
    }
  }

  private def validateOption(expr:Expression, choiceType:DefinedChoiceType, executionResult: TemplateExecutionResult): Result[Unit] = {
    expr.expressionType(executionResult).flatMap { exprType =>
      expr match {
        case StringConstant(str, _) =>
          choiceType.choices.values.find(_ === str) match {
            case Some(_) =>
              Success.unit
            case None =>
              Failure(s"the value $str is not part of the Choice type ${choiceType.typeName}")
          }
        case _ if exprType === choiceType =>
          Success.unit
        case _ =>
          Failure(s"the options need to be of type Text to possibly be of type ${expr.expressionType(executionResult)}")
      }
    }
  }

  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    name.variables(executionResult)

  override def missingInput(executionResult:TemplateExecutionResult): Result[Seq[VariableName]] = {
    val eitherMissing = name.missingInput(executionResult)
    val eitherMissingFromParameters = defaultValue
      .map(getMissingValuesFromParameter(executionResult, _)).getOrElse(Success(Seq()))

    for {
      missing <- eitherMissing
      missingFromParameters <- eitherMissingFromParameters
    } yield missing ++ missingFromParameters
  }


  private def getMissingValuesFromParameter(executionResult:TemplateExecutionResult, param:Parameter): Result[Seq[VariableName]] = param match {
    case OneValueParameter(expr) => expr.missingInput(executionResult)
    case ListParameter(exprs) => VariableType.sequence(exprs.map(_.missingInput(executionResult))).map(_.flatten)
    case Parameters(params) => VariableType.sequence(params.map({case (_,value) => getMissingValuesFromParameter(executionResult, value)})).map(_.flatten)
    case MappingParameter(mapping) => VariableType.sequence(mapping.values.map(_.missingInput(executionResult)).toSeq).map(_.flatten)
  }

  override def toString: String = name.name + variableTypeDefinition.map(definition => ":" + definition.name).getOrElse("")
}
