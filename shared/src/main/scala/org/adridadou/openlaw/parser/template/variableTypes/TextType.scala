package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import org.adridadou.openlaw.parser.template.{Divide, FormatterDefinition, TemplateExecutionResult, ValueOperation}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, UppercaseFormatter}
import org.adridadou.openlaw.result.Result


case object TextType extends VariableType("Text") {
  override def cast(value: String, executionResult: TemplateExecutionResult): String = value

  override def plus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Result[Option[String]] =
    (for {
      leftValue <- optLeft
      rightValue <- optRight
    } yield {
      for {
        left <- VariableType.convert[String](leftValue)
        right <- VariableType.convert[String](rightValue)
      } yield left + right
    }).sequence


  override def divide(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Result[Option[Any]] =
    (for {
      leftValue <- optLeft
      rightValue <- optRight
    } yield {
      for {
        left <- VariableType.convert[String](leftValue)
        right <- VariableType.convert[String](rightValue)
      } yield TemplatePath(Seq(left, right))
    }).sequence

  override def operationWith(rightType: VariableType, operation: ValueOperation): VariableType = operation match {
    case Divide =>
      TemplatePathType
    case _ =>
      TextType
  }

  override def internalFormat(value: Any): Result[String] = VariableType.convert[String](value)

  override def checkTypeName(nameToCheck: String): Boolean =
    Seq("Text", "String").exists(_.equalsIgnoreCase(nameToCheck))

  override def getFormatter(formatter: FormatterDefinition, executionResult: TemplateExecutionResult):Formatter = formatter.name.toLowerCase() match {
    case "uppercase" => new UppercaseFormatter
    case _ => defaultFormatter
  }

  def thisType: VariableType = TextType

  override def getTypeClass: Class[String] = classOf[String]

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case TextType => true
    case _ => otherType.isCompatibleType(this, operation)
  }
}
