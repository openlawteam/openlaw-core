package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.parser.template.{AgreementElement, FreeText, TemplateExecutionResult, Text}
import org.adridadou.openlaw.parser.template.variableTypes.VariableType
import org.adridadou.openlaw.result.{attempt, Failure, Result}

/**
  * Created by davidroon on 12.06.17.
  */
class UppercaseFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = VariableType.convert[String](value) map {
    case str => Seq(FreeText(Text(str.toUpperCase)))
  }
}


