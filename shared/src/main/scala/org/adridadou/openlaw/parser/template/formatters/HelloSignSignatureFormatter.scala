package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.{
  ExternalSignature,
  Identity
}
import org.adridadou.openlaw.parser.template.{
  AgreementElement,
  SignaturePlaceholder,
  TemplateExecutionResult
}
import org.adridadou.openlaw.result.Implicits.RichOption
import org.adridadou.openlaw.result.{Failure, Result, Success}

class HelloSignSignatureFormatter() extends Formatter {

  // See https://app.hellosign.com/api/textTagsWalkthrough
  val replacementText = "[sig|req|signer1]"

  // Empty as hello sign will render the content after the user has signed via hellosign
  override def format(
      expression: Expression,
      value: openlaw.OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] = {

    value match {
      case ExternalSignature(None, serviceName) =>
        // Field not yet filled in, so we use this as a placeholder for now
        Success(List(SignaturePlaceholder("[sig|req|signer1]")))
      case ExternalSignature(Some(identity), serviceName) =>
        for {
          index <- getIdentityIndex(identity, executionResult)
        } yield {
          List(SignaturePlaceholder(s"[sig|req|signer$index]"))
        }
      case a =>
        Failure(
          s"Value is not an external signature as expected, but is a ${a.getClass}. Value: ${a}"
        )
    }

  }

  def getIdentityIndex(
      identity: Identity,
      executionResult: TemplateExecutionResult
  ): Result[Int] = {

    executionResult
      .allIdentities()
      .flatMap(
        _.zipWithIndex
          .find({
            case (id, _) =>
              id.email == identity.email
          })
          .map(_._2 + 1)
          .toResult(s"Could not find ${identity} in execution result")
      )
  }

  // Empty as hello sign will render the content after the user has signed via hellosign
  override def stringFormat(
      expression: Expression,
      value: openlaw.OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    Success(replacementText)

  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(SignaturePlaceholder(replacementText))
}
