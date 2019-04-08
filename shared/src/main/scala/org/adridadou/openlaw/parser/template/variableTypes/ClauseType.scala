
package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template.{Parameter, TemplateExecutionResult}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.Result

case object ClauseType extends VariableType("Clause") with NoShowInForm {

  override def getTypeClass: Class[_ <: TemplateDefinition ] = classOf[TemplateDefinition]

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[Any]] = TemplateType
    .construct(constructorParams, executionResult)

  override def cast(value: String, executionResult: TemplateExecutionResult): TemplateDefinition = TemplateType.cast(value, executionResult)
  override def internalFormat(value: Any): String = TemplateType.internalFormat(value)
  override def defaultFormatter: Formatter = new NoopFormatter

  def thisType: VariableType = ClauseType
}