package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.{
  ExternalSignature,
  ExternalSignatureType,
  Identity
}
import org.adridadou.openlaw.result.{Failure, Result, Success}

/**
  * Created by davidroon on 12.06.17.
  */
class SignatureFormatter extends Formatter {

  private def formatExternalSignature(
      expression: Expression,
      externalSignature: ExternalSignature
  ): Result[List[AgreementElement]] = {
    externalSignature.identity match {
      case None           => Success(missingValueFormat(expression))
      case Some(identity) => Success(Nil)
    }
  }

  private def formatExternalSignatureString(
      expression: Expression,
      externalSignature: ExternalSignature
  ): Result[String] = {
    externalSignature.identity match {
      case None           => Success(missingSignatureText(expression))
      case Some(identity) => Success("")
    }
  }

  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] = value match {
    case externalSignature: ExternalSignature =>
      formatExternalSignature(expression, externalSignature)
    case identity: Identity =>
      executionResult
        .getSignatureProof(identity)
        .map(proof => {
          Success(
            List(
              FreeText(Text(s"/s/ ${proof.fullName}  (")),
              proof.validationLink,
              FreeText(Text(")"))
            )
          )
        })
        .getOrElse(Success(missingValueFormat(expression)))

    case other =>
      Failure(
        "invalid type " + other.getClass.getSimpleName + ". expecting Identity"
      )
  }

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] = {
    value match {
      case identity: Identity =>
        executionResult
          .getSignatureProof(identity)
          .map(proof => Success(s"/s/ ${proof.fullName}"))
          .getOrElse(Success(missingSignatureText(expression)))
      case externalSignature: ExternalSignature =>
        formatExternalSignatureString(expression, externalSignature)
      case other =>
        Failure(
          "invalid type " + other.getClass.getSimpleName + ". expecting Identity"
        )
    }
  }

  private def missingSignatureText(expression: Expression): String = {
    s"{{signature of $expression}}"
  }

  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(missingSignatureText(expression))))
}
