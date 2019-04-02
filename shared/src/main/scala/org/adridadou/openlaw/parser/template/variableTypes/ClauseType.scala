
package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template.{TemplateExecutionResult}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}

case object ClauseType extends VariableType("Clause") with NoShowInForm {

  override def getTypeClass: Class[_ <: ClauseType.type ] = this.getClass

  override def cast(value: String, executionResult: TemplateExecutionResult): TemplateDefinition = handleEither(decode[TemplateDefinition](value))
  override def internalFormat(value: Any): String = VariableType.convert[TemplateDefinition](value).asJson.noSpaces
  override def defaultFormatter: Formatter = new NoopFormatter

  def thisType: VariableType = ClauseType
}