package org.adridadou.openlaw.parser.template.formatters

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.Identity
import org.adridadou.openlaw.result.{Failure, Result, Success}

/**
  * Created by davidroon on 12.06.17.
  */
class SignatureFormatter extends Formatter {
  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] = value match {
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
        .getOrElse(Success(Nil))
    case other =>
      Failure(
        "invalid type " + other.getClass.getSimpleName + ". expecting Identity"
      )
  }

  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    value match {
      case identity: Identity =>
        executionResult
          .getSignatureProof(identity)
          .map(proof => Success(s"/s/ ${proof.fullName}"))
          .getOrElse(Success(missingValueString(expression)))
      case other =>
        Failure(
          "invalid type " + other.getClass.getSimpleName + ". expecting Identity"
        )
    }

  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(s"{{signature of $expression}}")))
}
