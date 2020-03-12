package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.{
  AgreementElement,
  FreeText,
  TemplateExecutionResult,
  Text
}
import org.adridadou.openlaw.parser.template.variableTypes.VariableType
import org.adridadou.openlaw.result.Result

/**
  * Created by davidroon on 12.06.17.
  */
class UppercaseFormatter extends Formatter {
  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    VariableType.convert[OpenlawString](value) map (
        str => List(FreeText(Text(str.toUpperCase)))
    )

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    VariableType.convert[OpenlawString](value) map (str => str.toUpperCase)

  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(s"[[$expression]]")))
}
