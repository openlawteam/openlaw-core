package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template.{AgreementElement, FreeText, TemplateExecutionResult, Text}
import org.adridadou.openlaw.result.{Result, Success}

/**
  * Created by davidroon on 09.06.17.
  */
trait Formatter {
  def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]]
}

class DefaultFormatter extends Formatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    Success(
      Seq(
        FreeText(
          Text(Option(value).map(_.toString).getOrElse(""))
        )
      )
    )
}