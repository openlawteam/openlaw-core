package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.{
  AgreementElement,
  FreeText,
  TemplateExecutionResult,
  Text
}
import org.adridadou.openlaw.parser.template.variableTypes.VariableType
import org.adridadou.openlaw.result.{Failure, Result, attempt}

/**
  * Created by davidroon on 12.06.17.
  */
class UppercaseFormatter extends Formatter {
  override def format(
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    VariableType.convert[OpenlawString](value) map (
        str => List(FreeText(Text(str.toUpperCase)))
    )

  override def missingValueFormat(
                                   name: String
                                 ): List[AgreementElement] =
    List(FreeText(Text(s"[[$name]]")))
}
