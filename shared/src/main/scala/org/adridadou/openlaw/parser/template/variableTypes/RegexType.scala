package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString}
import org.adridadou.openlaw.parser.template.{
  OneValueParameter,
  Parameter,
  TemplateExecutionResult,
  VariableMemberKey,
  VariableName
}
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression

import scala.util.matching.Regex

final case class RegexValue(value: Regex) extends OpenlawNativeValue

object RegexType extends VariableType("Regex") with NoShowInForm {
  override def getTypeClass: Class[_ <: openlaw.OpenlawValue] =
    classOf[RegexValue]

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[openlaw.OpenlawValue]] = {
    constructorParams match {
      case OneValueParameter(expr) =>
        for {
          paramType <- expr.expressionType(executionResult)
          _ <- if (paramType === TextType) Success.unit
          else
            Failure(
              s"parameter type should be text, instead it is ${paramType.name}"
            )
          optValue <- expr.evaluateT[OpenlawString](executionResult)
          regexValue <- optValue.map(value => attempt(value.r)).sequence
        } yield regexValue.map(RegexValue)
      case _ => Failure("regex only accepts one parameter as a constructor ")
    }
  }

  override def access(
      value: openlaw.OpenlawValue,
      variableName: VariableName,
      keys: List[VariableMemberKey],
      executionResult: TemplateExecutionResult
  ): Result[Option[openlaw.OpenlawValue]] = {
    keys match {
      case Nil => Success(Some(value))
      case VariableMemberKey(
            Right(OLFunctionCall(VariableName("match"), parameter))
          ) :: Nil =>
        for {
          regex <- VariableType.convert[RegexValue](value)
          optValue <- parameter.evaluateT[OpenlawString](executionResult)
        } yield optValue.map(v => regex.value.findFirstMatchIn(v).isDefined)
      case _ => super.access(value, variableName, keys, executionResult)
    }
  }

  override def validateKeys(
      variableName: VariableName,
      keys: List[VariableMemberKey],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] = {
    keys match {
      case Nil => Success.unit
      case VariableMemberKey(
            Right(OLFunctionCall(VariableName("match"), parameter))
          ) :: Nil =>
        for {
          expressionType <- parameter.expressionType(executionResult)
          _ <- if (expressionType === TextType) Success.unit
          else
            Failure(
              s"function 'match' takes a Text as parameter, not ${expressionType.name}"
            )
        } yield ()
      case _ =>
        super.validateKeys(variableName, keys, expression, executionResult)
    }
  }

  override def keysType(
      keys: List[VariableMemberKey],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[VariableType] = {
    keys match {
      case Nil => Success(RegexType)
      case VariableMemberKey(Right(OLFunctionCall(VariableName("match"), _))) :: Nil =>
        Success(YesNoType)
      case _ => super.keysType(keys, expression, executionResult)
    }
  }

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[openlaw.OpenlawValue] =
    attempt(value.r).map(RegexValue)

  override def internalFormat(value: openlaw.OpenlawValue): Result[String] =
    value match {
      case RegexValue(value) => Success(value.toString())
      case other =>
        Failure(
          s"should be Regex but instead was ${other.getClass.getSimpleName}"
        )
    }

  override def thisType: VariableType = RegexType
}
