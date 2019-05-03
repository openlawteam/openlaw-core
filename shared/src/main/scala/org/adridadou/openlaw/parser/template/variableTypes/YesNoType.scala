package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.{BooleanOpenlawValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.TemplateExecutionResult

case object YesNoType extends VariableType("YesNo") {
  override def cast(value: String, executionResult: TemplateExecutionResult): BooleanOpenlawValue = value.toBoolean
  override def internalFormat(value: OpenlawValue): String = value.toString

  override def checkTypeName(nameToCheck: String): Boolean = Seq("YesNo","Boolean", "Bool").exists(_.equalsIgnoreCase(nameToCheck))

  override def getTypeClass: Class[_ <: YesNoType.type ] = this.getClass

  def thisType: VariableType = YesNoType
}
