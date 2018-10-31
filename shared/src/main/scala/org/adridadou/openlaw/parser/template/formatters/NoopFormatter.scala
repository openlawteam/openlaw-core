package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.parser.template.{AgreementElement, TemplateExecutionResult}

/**
  * Created by davidroon on 12.06.17.
  */
class NoopFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Either[String, Seq[AgreementElement]] = Right(Seq())
}


