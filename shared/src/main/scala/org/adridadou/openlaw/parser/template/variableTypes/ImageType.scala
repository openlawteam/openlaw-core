package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success}

object ImageFormatter extends Formatter {
  def format(value:OpenlawValue, executionResult: TemplateExecutionResult): Result[List[AgreementElement]] = value match {
    case OpenlawString(url) => Success(List(ImageElement(url)))
    case _ => Failure("unsupported image value found: $value")
  }
}

case object ImageType extends VariableType("Image") {
  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OpenlawString] = Success(value)

  override def internalFormat(value: OpenlawValue): Result[String] = VariableType.convert[OpenlawString](value)

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[OpenlawString]] = constructorParams match {
    case OneValueParameter(expr) =>
      expr
        .evaluate(executionResult)
        .flatMap(opt => opt.map(value => VariableType.convert[OpenlawString](value).map(OpenlawString)).sequence)
    case _ => Failure("constructor only handles single value")
  }

  def thisType: VariableType = ImageType

  override def defaultFormatter: Formatter = ImageFormatter

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case ImageType => true
    case _ => otherType.isCompatibleType(this, operation)
  }

  override def getTypeClass: Class[OpenlawString] = classOf[OpenlawString]
}
