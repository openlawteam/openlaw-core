package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.{Parameter, Parameters, TemplateExecutionResult}
import org.adridadou.openlaw.parser.template.expressions.Expression
import io.circe.syntax._
import io.circe.parser._
import cats.implicits._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}

case object ValidationType extends VariableType(name = "Validation") with NoShowInForm {

  implicit val validationEnc: Encoder[Validation] = deriveEncoder[Validation]
  implicit val validationDec: Decoder[Validation] = deriveDecoder[Validation]

  override def cast(value: String, executionResult: TemplateExecutionResult): Validation = decode[Validation](value) match {
    case Right(result) => result
    case Left(ex) => throw new RuntimeException(ex.getMessage)
  }

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Either[Throwable, Option[Any]] = constructorParams match {
    case Parameters(v) =>
      val values = v.toMap
      validate(Validation(
        condition = getExpression(values, "condition"),
        errorMessage = getExpression(values, "errorMessage")
      ), executionResult).map(Some(_))

    case _ => Left(new Exception("Validation need to get 'condition' and 'errorMessage' as constructor parameter"))
  }
  override def internalFormat(value: Any): String = VariableType.convert[Validation](value).asJson.noSpaces

  override def getTypeClass: Class[_ <: ValidationType.type ] = this.getClass


  override def defaultFormatter: Formatter = new NoopFormatter

  private def validate(validation:Validation, executionResult: TemplateExecutionResult):Either[Throwable, Validation] = {
    val conditionType = validation.condition.expressionType(executionResult)
    val errorMessageType = validation.errorMessage.expressionType(executionResult)
    if(conditionType =!= YesNoType){
      Left(new Exception(s"the condition expression of a validation needs to be of type YesNo, instead it is ${conditionType.name}"))
    } else if(errorMessageType =!= TextType) {
      Left(new Exception(s"The error message expression of a validation needs to be of type String, instead it is ${errorMessageType.name}"))
    } else {
      Right(validation)
    }
  }

  def thisType: VariableType = ValidationType
}

case class Validation(condition:Expression, errorMessage:Expression) {
  def validate(executionResult: TemplateExecutionResult):Option[String] = {
    condition.evaluate(executionResult).map(VariableType.convert[Boolean])
      .filter(_ === false)
      .map(_ => errorMessage
        .evaluate(executionResult).map(VariableType.convert[String])
        .getOrElse(s"validation error (error message could not be resolved)")
      )
  }
}