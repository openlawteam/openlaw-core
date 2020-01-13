package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.Identity
import org.adridadou.openlaw.result.{Failure, Result, Success}

/**
  * Created by davidroon on 12.06.17.
  */
class SignatureFormatter extends Formatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[List[AgreementElement]] = value match {
    case identity:Identity =>
      executionResult.getSignatureProof(identity).map(proof => {
        Success(List(
          FreeText(Text(s"/s/ ${proof.fullName}  (")),
          proof.validationLink,
          FreeText(Text(")"))
        ))
      }).getOrElse(Right(Nil))
    case other =>
      Failure("invalid type " + other.getClass.getSimpleName + ". expecting Identity")
  }
}