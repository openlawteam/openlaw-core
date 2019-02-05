package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.parser.template.{AgreementElement, TemplateExecutionResult}
import org.adridadou.openlaw.result.{Result, Success}

/**
  * Created by davidroon on 12.06.17.
  */
class NoopFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = Success(Seq())
}


