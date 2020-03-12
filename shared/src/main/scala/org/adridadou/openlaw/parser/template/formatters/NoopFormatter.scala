package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.{
  AgreementElement,
  TemplateExecutionResult
}
import org.adridadou.openlaw.result.{Result, Success}

/**
  * Created by davidroon on 12.06.17.
  */
class NoopFormatter extends Formatter {
  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] = Success(Nil)

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    Success("")
  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    Nil
}
