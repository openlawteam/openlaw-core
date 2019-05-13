package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.{Divide, FormatterDefinition, TemplateExecutionResult, ValueOperation}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, UppercaseFormatter}


case object LargeTextType extends VariableType("LargeText") {

  override def cast(value: String, executionResult: TemplateExecutionResult): OpenlawString = value

  override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[OpenlawValue] = for(
    leftValue <- optLeft;
    rightValue <- optRight
  ) yield VariableType.convert[OpenlawString](leftValue).string + VariableType.convert[OpenlawString](rightValue).string


  override def divide(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Option[OpenlawValue] = {
    for {
      left <- optLeft.map(VariableType.convert[OpenlawString])
      right <- optRight.map(VariableType.convert[OpenlawString])
    } yield TemplatePath(Seq(left, right))
  }

  override def operationWith(rightType: VariableType, operation: ValueOperation): VariableType = operation match {
    case Divide =>
      TemplatePathType
    case _ =>
      TextType
  }

  override def internalFormat(value: OpenlawValue): String = VariableType.convert[OpenlawString](value)

  override def getTypeClass = classOf[String]

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
