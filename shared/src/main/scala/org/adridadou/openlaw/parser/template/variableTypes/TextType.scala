package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.{
  Divide,
  FormatterDefinition,
  TemplateExecutionResult,
  ValueOperation
}
import org.adridadou.openlaw.parser.template.formatters.{
  Formatter,
  UppercaseFormatter
}
import org.adridadou.openlaw.result.{Result, Success}
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression

case object TextType extends VariableType("Text") {
  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawString] = Success(value)

  override def plus(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawString]] =
    for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
      leftType <- left.expressionType(executionResult)
      rightType <- right.expressionType(executionResult)
      leftString <- leftValue
        .map(leftType.defaultFormatter.stringFormat(left, _, executionResult))
        .getOrElse(Success(leftType.defaultFormatter.missingValueString(left)))
      rightString <- rightValue
        .map(rightType.defaultFormatter.stringFormat(right, _, executionResult))
        .getOrElse(
          Success(rightType.defaultFormatter.missingValueString(right))
        )
    } yield Some(OpenlawString(leftString + rightString))

  override def divide(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[TemplatePath]] = {
    optRight match {
      case Some(_: TemplatePath) =>
        combineConverted[OpenlawString, TemplatePath, TemplatePath](
          optLeft,
          optRight
        ) {
          case (left, right) => Success(TemplatePath(left :: right.path))
        }
      case _ =>
        combineConverted[OpenlawString, TemplatePath](optLeft, optRight) {
          case (left, right) => Success(TemplatePath(List(left, right)))
        }
    }

  }

  override def operationWith(
      rightType: VariableType,
      operation: ValueOperation
  ): VariableType = operation match {
    case Divide =>
      TemplatePathType
    case _ =>
      TextType
  }

  override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType.convert[OpenlawString](value)

  override def checkTypeName(nameToCheck: String): Boolean =
    Seq("Text", "String").exists(_.equalsIgnoreCase(nameToCheck))

  override def getFormatter(
      formatter: FormatterDefinition,
      executionResult: TemplateExecutionResult
  ): Result[Formatter] = formatter.name.toLowerCase() match {
    case "uppercase" => Success(new UppercaseFormatter)
    case _           => Success(defaultFormatter)
  }

  def thisType: VariableType = TextType

  override def getTypeClass: Class[OpenlawString] = classOf[OpenlawString]

  override def isCompatibleType(
      otherType: VariableType,
      operation: ValueOperation
  ): Boolean = otherType match {
    case TextType => true
    case _        => otherType.isCompatibleType(this, operation)
  }
}
