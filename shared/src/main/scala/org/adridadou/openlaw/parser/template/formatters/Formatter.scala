package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.{
  AgreementElement,
  FreeText,
  TemplateExecutionResult,
  Text
}
import org.adridadou.openlaw.result.{Result, Success}

/**
  * Created by davidroon on 09.06.17.
  */
trait Formatter {
  def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]]

  def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String]

  def missingValueFormat(expression: Expression): List[AgreementElement]

  def missingValueString(expression: Expression): String =
    s"[[$expression]]"
}

object DefaultFormatter extends Formatter {
  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    Success(
      List(
        FreeText(
          Text(Option(value).map(_.toString).getOrElse(""))
        )
      )
    )
  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(s"[[$expression]]")))

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    Success(
      Option(value).map(_.toString).getOrElse(missingValueString(expression))
    )
}
