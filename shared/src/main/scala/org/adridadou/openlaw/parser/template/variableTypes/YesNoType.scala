package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.result.{Result, Success, attempt}

case object YesNoType extends VariableType("YesNo") {
  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawBoolean] = attempt(value.toBoolean)
  override def internalFormat(value: OpenlawValue): Result[String] =
    Success(value.toString)

  override def checkTypeName(nameToCheck: String): Boolean =
    Seq("YesNo", "Boolean", "Bool").exists(_.equalsIgnoreCase(nameToCheck))

  override def getTypeClass: Class[OpenlawBoolean] = classOf[OpenlawBoolean]

  def thisType: VariableType = YesNoType
}
