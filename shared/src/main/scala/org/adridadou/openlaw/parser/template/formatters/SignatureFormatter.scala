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

  val hellosignFormatter = new HelloSignSignatureFormatter()

  private def formatExternalSignature(
      expression: Expression
  ): Result[List[AgreementElement]] = {
    Success(List(SignaturePlaceholder(missingSignatureText(expression))))
  }

  private def formatExternalSignatureString(
      expression: Expression
  ): Result[String] = {
    Success(missingSignatureText(expression))
  }

  def isHelloSign(externalSignature: ExternalSignature) = {
    externalSignature.serviceName.serviceName.toLowerCase() == "hellosign"
  }

  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] = value match {
    case externalSignature: ExternalSignature
        if isHelloSign(externalSignature) => {
      hellosignFormatter.format(expression, value, executionResult)
    }
    case externalSignature: ExternalSignature =>
      // External signatures are just placeholder text until the external service replaces them with the signature
      // applied by the user
      formatExternalSignature(expression)
    case identity: Identity =>
      executionResult
        .getSignatureProof(identity)
        .map(proof => {
          Success(
            List(
              FreeText(Text(s"/s/ ${proof.fullName}"))
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
      case externalSignature: ExternalSignature
          if isHelloSign(externalSignature) =>
        hellosignFormatter.stringFormat(expression, value, executionResult)
      case externalSignature: ExternalSignature =>
        formatExternalSignatureString(expression)
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
