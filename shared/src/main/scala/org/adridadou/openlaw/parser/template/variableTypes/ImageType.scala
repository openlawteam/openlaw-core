package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template.{AgreementElement, FormatterDefinition, ImageElement, OneValueParameter, Parameter, TemplateExecutionResult, ValueOperation}

object ImageFormatter extends Formatter {
  def format(value:Any, executionResult: TemplateExecutionResult): Either[String, Seq[AgreementElement]] = value match {
    case url: String => Right(Seq(ImageElement(url)))
    case _ => Left(s"unsupported image value found: $value")
  }
}

case object ImageType extends VariableType("Image") {
  override def cast(value: String, executionResult: TemplateExecutionResult): String = value

  override def internalFormat(value: Any): String = VariableType.convert[String](value)

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Option[String] = constructorParams match {
    case OneValueParameter(expr) => expr.evaluate(executionResult).map(value => VariableType.convert[String](value))
    case _ => throw new RuntimeException("constructor only handles single value")
  }

  def thisType: VariableType = ImageType

  override def defaultFormatter: Formatter = ImageFormatter

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case ImageType => true
    case _ => otherType.isCompatibleType(this, operation)
  }
}
