package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import org.adridadou.openlaw.{OpenlawLink, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case object LinkType extends VariableType("Link") {
  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OpenlawLink] = attempt(Link("ol link", value))

  override def internalFormat(value: OpenlawValue): Result[String] = VariableType.convert[OpenlawLink](value).map(_.toString)

  override def construct(constructorParams:Parameter, executionResult:TemplateExecutionResult): Result[Option[OpenlawLink]] = constructorParams match {
    case OneValueParameter(expr) =>
      expr
        .evaluate(executionResult)
        .flatMap(opt => opt.map(value => VariableType.convert[OpenlawLink](value).map(OpenlawLink(_))).sequence)
    case _ => Failure("constructor only handles single value")
  }

  def thisType: VariableType = LinkType

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case LinkType => true
    case _ => otherType.isCompatibleType(this, operation)
  }

  override def getTypeClass: Class[OpenlawLink] = classOf[OpenlawLink]
}
