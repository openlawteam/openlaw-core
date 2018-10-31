package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.parser.template.{AgreementElement, FreeText, TemplateExecutionResult, Text}
import org.adridadou.openlaw.parser.template.variableTypes.VariableType

import scala.util.{Failure, Success, Try}

/**
  * Created by davidroon on 12.06.17.
  */
class UppercaseFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Either[String, Seq[AgreementElement]] = Try(VariableType.convert[String](value)) match {
    case Success(str) =>
      Right(Seq(FreeText(Text(str.toUpperCase))))
    case Failure(ex) => Left(ex.getMessage)
  }
}


