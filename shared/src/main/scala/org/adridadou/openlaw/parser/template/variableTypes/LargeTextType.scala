package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
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

case object LargeTextType extends VariableType("LargeText") {

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawString] = Success(value)

  override def plus(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] = TextType.plus(left, right, executionResult)

  override def divide(
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    TextType.divide(left, right, executionResult)

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

  override def getTypeClass: Class[OpenlawString] = classOf[OpenlawString]

  override def getFormatter(
      formatter: FormatterDefinition,
      executionResult: TemplateExecutionResult
  ): Result[Formatter] = formatter.name.toLowerCase() match {
    case "uppercase" => Success(new UppercaseFormatter)
    case _           => Success(defaultFormatter)
  }

  def thisType: VariableType = TextType

  override def isCompatibleType(
      otherType: VariableType,
      operation: ValueOperation
  ): Boolean = otherType match {
    case TextType => true
    case _        => otherType.isCompatibleType(this, operation)
  }
}
