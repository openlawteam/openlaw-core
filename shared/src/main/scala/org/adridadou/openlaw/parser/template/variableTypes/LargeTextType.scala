package org.adridadou.openlaw.parser.template.variableTypes

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
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    combineConverted[OpenlawString, OpenlawValue](optLeft, optRight) {
      case (left, right) => Success(left + right)
    }

  override def divide(
      optLeft: Option[OpenlawValue],
      optRight: Option[OpenlawValue],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    combineConverted[OpenlawString, OpenlawValue](optLeft, optRight) {
      case (left, right) => Success(TemplatePath(List(left, right)))
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

  override def getTypeClass: Class[OpenlawString] = classOf[OpenlawString]

  override def checkTypeName(nameToCheck: String): Boolean =
    Seq("LargeText", "String").exists(_.equalsIgnoreCase(nameToCheck))

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
