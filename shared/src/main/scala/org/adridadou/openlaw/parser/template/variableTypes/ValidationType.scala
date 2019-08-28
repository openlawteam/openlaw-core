package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.{Parameter, Parameters, TemplateExecutionResult}
import org.adridadou.openlaw.parser.template.expressions.Expression
import io.circe.syntax._
import io.circe.parser._
import cats.implicits._
import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, ResultNel, Success}
import org.adridadou.openlaw.result.Implicits._

case object ValidationType extends VariableType(name = "Validation") with NoShowInForm {

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[Validation] = decode[Validation](value) match {
    case Right(result) => Success(result)
    case Left(ex) => Failure(ex)
  }

	def construct2(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Validation] = constructorParams match {
		case Parameters(v) =>
			val values = v.toMap
			for {
				condition <- getExpression(values, "condition")
				errorMessage <- getExpression(values, "errorMessage")
				result <- validate(Validation(
					condition = condition,
					errorMessage = errorMessage
				), executionResult)
			} yield result

		case _ => Failure("Validation need to get 'condition' and 'errorMessage' as constructor parameter")
	}

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[Validation]] =
		construct2(constructorParams, executionResult).map(Some(_))

  override def internalFormat(value: OpenlawValue): Result[String] = VariableType.convert[Validation](value).map(_.asJson.noSpaces)

  override def getTypeClass: Class[_ <: Validation ] = classOf[Validation]

  override def defaultFormatter: Formatter = new NoopFormatter

  private def validate(validation:Validation, executionResult: TemplateExecutionResult): Result[Validation] =
    for {
      conditionType <- validation.condition.expressionType(executionResult)
      errorMessageType <- validation.errorMessage.expressionType(executionResult)
      result <- {
        if (conditionType =!= YesNoType) {
          Failure(s"the condition expression of a validation needs to be of type YesNo, instead it is ${conditionType.name}")
        } else if (errorMessageType =!= TextType) {
          Failure(s"The error message expression of a validation needs to be of type String, instead it is ${errorMessageType.name}")
        } else {
          Success(validation)
        }
      }
    } yield result

  def thisType: VariableType = ValidationType
}

case object Validation {
  implicit val validationEnc: Encoder[Validation] = deriveEncoder
  implicit val validationDec: Decoder[Validation] = deriveDecoder
}

final case class Validation(condition:Expression, errorMessage:Expression) extends OpenlawNativeValue {
  def validate(executionResult: TemplateExecutionResult):ResultNel[Unit] = (for {
      value <- condition.evaluateT[OpenlawBoolean](executionResult)
			evaluatedErrorMessage <- errorMessage.evaluateT[OpenlawString](executionResult)
			_ <- value
				.filter(_ === false)
				.map(_ => Failure(evaluatedErrorMessage.getOrElse(s"validation error (error message could not be resolved)")))
				.getOrElse(Success(()))
    } yield ()).toResultNel
}
