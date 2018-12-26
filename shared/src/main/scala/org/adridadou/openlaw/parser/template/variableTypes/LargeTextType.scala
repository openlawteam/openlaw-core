package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template.{Divide, FormatterDefinition, TemplateExecutionResult, ValueOperation}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, UppercaseFormatter}


case object LargeTextType extends VariableType("LargeText") {
  override def cast(value: String, executionResult: TemplateExecutionResult): String = value

  override def plus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[String] = for(
    leftValue <- optLeft;
    rightValue <- optRight
  ) yield VariableType.convert[String](leftValue) + VariableType.convert[String](rightValue)


  override def divide(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Any] = {
    for {
      left <- optLeft.map(VariableType.convert[String])
      right <- optRight.map(VariableType.convert[String])
    } yield TemplatePath(Seq(left, right))
  }

  override def operationWith(rightType: VariableType, operation: ValueOperation): VariableType = operation match {
    case Divide =>
      TemplatePathType
    case _ =>
      TextType
  }

  override def internalFormat(value: Any): String = VariableType.convert[String](value)

  override def checkTypeName(nameToCheck: String): Boolean =
    Seq("LargeText", "String").exists(_.equalsIgnoreCase(nameToCheck))

  override def getFormatter(formatter: FormatterDefinition, executionResult: TemplateExecutionResult):Formatter = formatter.name.toLowerCase() match {
    case "uppercase" => new UppercaseFormatter
    case _ => defaultFormatter
  }

  def thisType: VariableType = TextType

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case TextType => true
    case _ => otherType.isCompatibleType(this, operation)
  }
}
