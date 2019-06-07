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
import org.adridadou.openlaw.result.{Failure, Result, Success}

case object ValidationType extends VariableType(name = "Validation") with NoShowInForm {

  implicit val validationEnc: Encoder[Validation] = deriveEncoder[Validation]
  implicit val validationDec: Decoder[Validation] = deriveDecoder[Validation]

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[Validation] = decode[Validation](value) match {
    case Right(result) => Success(result)
    case Left(ex) => Failure(ex)
  }

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = constructorParams match {
    case Parameters(v) =>
      val values = v.toMap
      validate(Validation(
        condition = getExpression(values, "condition"),
        errorMessage = getExpression(values, "errorMessage")
      ), executionResult).map(Some(_))

    case _ => Failure("Validation need to get 'condition' and 'errorMessage' as constructor parameter")
  }
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

case class Validation(condition:Expression, errorMessage:Expression) extends OpenlawNativeValue {
  def validate(executionResult: TemplateExecutionResult):Result[Unit] =
    condition
      .evaluate(executionResult)
      .flatMap { option =>
        option.map(VariableType.convert[OpenlawBoolean](_)).sequence
      }
      .flatMap { booleanOption =>
        booleanOption
          .filter(_ === false)
          .map { _ =>
            errorMessage
              .evaluate(executionResult)
              .flatMap { stringOption =>
                stringOption
                  .map(VariableType.convert[OpenlawString](_))
                  .sequence
              }
              .map (_.getOrElse(s"validation error (error message could not be resolved)"))
          }
          .sequence
      }
      .map(_.map(Failure(_)).getOrElse(Success(())))
}
