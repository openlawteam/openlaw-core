package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.{
  Divide,
  FormatterDefinition,
  Plus,
  TemplateExecutionResult,
  ValueOperation
}
import org.adridadou.openlaw.parser.template.formatters.{
  Formatter,
  UppercaseFormatter
}
import org.adridadou.openlaw.result.{Result, Success}
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
      left: Expression,
      right: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Option[TemplatePath]] =
    for {
      leftValue <- left.evaluate(executionResult)
      rightValue <- right.evaluate(executionResult)
      leftType <- left.expressionType(executionResult)
      rightType <- right.expressionType(executionResult)
      leftPath <- leftValue match {
        case Some(path: TemplatePath) =>
          Success(path)
        case Some(str: OpenlawString) =>
          Success(TemplatePath(List(str.underlying)))
        case Some(otherValue) =>
          leftType.defaultFormatter
            .stringFormat(left, otherValue, executionResult)
            .map(str => TemplatePath(List(str)))
        case None =>
          Success(
            TemplatePath(
              List(leftType.defaultFormatter.missingValueString(left))
            )
          )
      }
      result <- rightValue match {
        case Some(path: TemplatePath) =>
          Success(Some(TemplatePath(leftPath.path ++ path.path)))
        case Some(other) =>
          rightType.defaultFormatter
            .stringFormat(right, other, executionResult)
            .map(str => Some(TemplatePath(leftPath.path ++ List(str))))
        case None =>
          Success(
            Some(
              TemplatePath(
                leftPath.path ++ List(
                  rightType.defaultFormatter.missingValueString(right)
                )
              )
            )
          )
      }
    } yield result

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

  override def typeNames: List[String] =
    List("Text", "String")

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
  ): Boolean = (otherType, operation) match {
    case (TextType, _) => true
    case (_, Plus)     => true
    case (_, _)        => otherType.isCompatibleType(this, operation)
  }
}
