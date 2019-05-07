package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.{BooleanOpenlawValue, OpenlawValue, StringOpenlawValue}
import org.adridadou.openlaw.parser.template.{Divide, FormatterDefinition, TemplateExecutionResult, ValueOperation}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, UppercaseFormatter}


case object TextType extends VariableType("Text") {
  override type T = StringOpenlawValue

  override def cast(value: String, executionResult: TemplateExecutionResult): StringOpenlawValue = value

  override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[StringOpenlawValue] = for(
    leftValue <- optLeft;
    rightValue <- optRight
  ) yield VariableType.convert[StringOpenlawValue](leftValue).get + VariableType.convert[StringOpenlawValue](rightValue).get


  override def divide(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[OpenlawValue] = {
    for {
      left <- optLeft.map(VariableType.convert[StringOpenlawValue])
      right <- optRight.map(VariableType.convert[StringOpenlawValue])
    } yield TemplatePath(Seq(left.get, right.get))
  }

  override def operationWith(rightType: VariableType, operation: ValueOperation): VariableType = operation match {
    case Divide =>
      TemplatePathType
    case _ =>
      TextType
  }

  override def internalFormat(value: OpenlawValue): String = VariableType.convert[StringOpenlawValue](value).get

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
