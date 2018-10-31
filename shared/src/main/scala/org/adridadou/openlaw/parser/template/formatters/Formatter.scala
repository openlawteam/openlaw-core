package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.parser.template.{AgreementElement, FreeText, TemplateExecutionResult, Text}

/**
  * Created by davidroon on 09.06.17.
  */
trait Formatter {
  def format(value:Any, executionResult: TemplateExecutionResult):Either[String, Seq[AgreementElement]]
}

class DefaultFormatter extends Formatter {
  override def format(value: Any, executionResult:TemplateExecutionResult): Either[String, Seq[AgreementElement]] =
    Right(
      Seq(
        FreeText(
          Text(Option(value).map(_.toString).getOrElse(""))
        )
      )
    )
}