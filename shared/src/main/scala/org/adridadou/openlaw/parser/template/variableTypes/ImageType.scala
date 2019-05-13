package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

object ImageFormatter extends Formatter {
  def format(value:OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = value match {
    case OpenlawString(url) => Success(Seq(ImageElement(url)))
    case _ => Failure("unsupported image value found: $value")
  }
}

case object ImageType extends VariableType("Image") {
  override def cast(value: String, executionResult: TemplateExecutionResult): OpenlawString = value

  override def internalFormat(value: OpenlawValue): String = VariableType.convert[OpenlawString](value)

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[OpenlawString]] = constructorParams match {
    case OneValueParameter(expr) =>
      attempt(expr.evaluate(executionResult).map(value => VariableType.convert[OpenlawString](value)))
    case _ => Failure("constructor only handles single value")
  }

  def thisType: VariableType = ImageType

  override def defaultFormatter: Formatter = ImageFormatter

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case ImageType => true
    case _ => otherType.isCompatibleType(this, operation)
  }

  override def getTypeClass = classOf[OpenlawString]
}
