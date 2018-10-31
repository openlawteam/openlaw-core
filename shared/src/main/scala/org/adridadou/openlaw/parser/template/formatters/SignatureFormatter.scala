package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.Identity

/**
  * Created by davidroon on 12.06.17.
  */
class SignatureFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Either[String, Seq[AgreementElement]] = value match {
    case identity:Identity =>
      executionResult.getSignatureProof(identity).map(proof => {
        Right(Seq(
          FreeText(Text(s"/s/ ${proof.fullName}  (")),
          proof.validationLink,
          FreeText(Text(")"))
        ))
      }).getOrElse(Right(Seq()))
    case other =>
      Left("invalid type " + other.getClass.getSimpleName + ". expecting Identity")
  }
}