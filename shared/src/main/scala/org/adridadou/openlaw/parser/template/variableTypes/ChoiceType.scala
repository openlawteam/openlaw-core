package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template._
import cats.implicits._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}

case class Choices(values: Seq[String])
object Choices {
  def apply(value: String): Choices = Choices(Seq(value))
}

case object ChoiceType extends VariableType("Choice") with TypeGenerator[Choices] {

  private implicit val enc: Encoder[Choices] = deriveEncoder[Choices]
  private implicit val dec: Decoder[Choices] = deriveDecoder[Choices]

  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Option[Choices] = param match {
    case OneValueParameter(value) =>
      Some(Choices(generateValues(Seq(value), executionResult)))
    case ListParameter(values) =>
      Some(Choices(generateValues(values, executionResult)))
    case _ => throw new RuntimeException("choice must have one or more expressions as constructor parameters")
  }

  private def generateValues(exprs:Seq[Expression], executionResult: TemplateExecutionResult):Seq[String] = {
    exprs.flatMap(_.evaluate(executionResult)).map(VariableType.convert[String])
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Choices = handleEither(decode[Choices](value))

  override def internalFormat(value: Any): String = value match {
    case call:Choices =>
      call.asJson.noSpaces
  }

  override def checkTypeName(nameToCheck: String): Boolean = Seq("Choice").exists(_.equalsIgnoreCase(nameToCheck))

  def thisType: VariableType = ChoiceType

  override def generateType(name: VariableName, choices: Choices): VariableType =
    DefinedChoiceType(choices, name.name)
}

case class DefinedChoiceType(choices:Choices, typeName:String) extends VariableType(name = typeName) {

  override def cast(value: String, executionResult: TemplateExecutionResult): Any = {
    choices.values
      .find(_ === value) match {
        case Some(result) =>
          result
        case None =>
          throw new RuntimeException(s"the value $value is not part of the type $typeName")
      }
  }

  override def internalFormat(value: Any): String = VariableType.convert[String](value)

  override def thisType: VariableType = this

  override def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean = otherType match {
    case TextType => true
    case LargeTextType => true
    case DefinedChoiceType(_, otherTypeName) if otherTypeName === typeName => true
    case _ => false
  }
}
