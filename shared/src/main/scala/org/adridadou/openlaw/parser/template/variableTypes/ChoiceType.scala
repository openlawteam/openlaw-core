package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template._
import cats.implicits._
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, attempt}

case class Choices(values: Seq[String]) extends OpenlawValue

object Choices {
  def apply(value: String): Choices = Choices(Seq(value))

  implicit val choicesEnc:Encoder[Choices] = deriveEncoder[Choices]
  implicit val choicesDec:Decoder[Choices] = deriveDecoder[Choices]
}

case object ChoiceType extends VariableType("Choice") with TypeGenerator[Choices] {

  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Result[Option[Choices]] = param match {
    case OneValueParameter(value) =>
      attempt(Some(Choices(generateValues(Seq(value), executionResult))))
    case ListParameter(values) =>
      attempt(Some(Choices(generateValues(values, executionResult))))
    case _ => Failure("choice must have one or more expressions as constructor parameters")
  }

  private def generateValues(exprs:Seq[Expression], executionResult: TemplateExecutionResult):Seq[String] = {
    exprs.flatMap(_.evaluate(executionResult)).map(VariableType.convert[OpenlawString](_).string)
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Choices = handleEither(decode[Choices](value))

  override def internalFormat(value: OpenlawValue): String = value match {
    case call:Choices =>
      call.asJson.noSpaces
  }

  override def checkTypeName(nameToCheck: String): Boolean = Seq("Choice").exists(_.equalsIgnoreCase(nameToCheck))

  override def getTypeClass: Class[_ <: ChoiceType.type ] = this.getClass

  def thisType: VariableType = ChoiceType

  override def generateType(name: VariableName, choices: Choices): VariableType =
    DefinedChoiceType(choices, name.name)
}

object DefinedChoiceType {
  implicit val definedChoiceTypeEnc:Encoder[DefinedChoiceType] = (a: DefinedChoiceType) => a.serialize
  implicit val definedChoiceTypeDec:Decoder[DefinedChoiceType] = (c: HCursor) => {
    for {
      name <- c.downField("name").as[String]
      values <- c.downField("values").as[Seq[String]]
    } yield DefinedChoiceType(choices = Choices(values), typeName = name)
  }
}

case class DefinedChoiceType(choices:Choices, typeName:String) extends VariableType(name = typeName) {


  override def serialize: Json = {
    Json.obj(
      "name" -> Json.fromString(typeName),
      "values" -> Json.arr(choices.values.map(v => Json.fromString(v)):_*))
  }

  override def cast(value: String, executionResult: TemplateExecutionResult): OpenlawValue = {
    choices.values
      .find(_ === value) match {
        case Some(result) =>
          result
        case None =>
          throw new RuntimeException(s"the value $value is not part of the type $typeName")
      }
  }

  override def internalFormat(value: OpenlawValue): String = VariableType.convert[OpenlawString](value)

  override def thisType: VariableType = this

  override def getTypeClass: Class[_ <: String] = classOf[String]

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case TextType => true
    case LargeTextType => true
    case DefinedChoiceType(_, otherTypeName) if otherTypeName === typeName => true
    case _ => false
  }
}
