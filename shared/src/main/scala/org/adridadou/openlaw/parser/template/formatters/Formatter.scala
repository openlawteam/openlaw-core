package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.OpenlawValue
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
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]]
  def missingValueFormat(name: String): List[AgreementElement]
}

object DefaultFormatter extends Formatter {
  override def format(
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
      name: String
  ): List[AgreementElement] =
    List(FreeText(Text(s"[[${name}]]")))
}
