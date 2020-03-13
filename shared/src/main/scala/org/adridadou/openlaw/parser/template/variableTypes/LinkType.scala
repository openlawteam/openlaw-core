package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}

object LinkInfo {
  implicit val linkInfoEnc: Encoder[LinkInfo] = deriveEncoder
  implicit val linkInfoDec: Decoder[LinkInfo] = deriveDecoder
}

final case class LinkInfo(label: String, url: String) extends OpenlawNativeValue

case object LinkType
    extends VariableType(name = "Link")
    with NoShowInFormButRender {

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[LinkInfo] = decode[LinkInfo](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType.convert[OpenlawString](value)

  override def defaultFormatter: Formatter = new LinkFormatter

  override def getTypeClass: Class[LinkInfo] = classOf[LinkInfo]

  override def checkTypeName(nameToCheck: String): Boolean =
    List("Link").exists(_.equalsIgnoreCase(nameToCheck))

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[LinkInfo]] =
    constructorParams match {
      case Parameters(seq) =>
        val map = seq.toMap
        for {
          label <- map.get("label").traverse(getOneValueConstant)
          url <- map.get("url").traverse(getOneValueConstant)
        } yield (label, url) mapN { LinkInfo(_, _) }
      case _ =>
        Failure("""Link requires parameters, not a unique value or a list""")
    }

  def thisType: VariableType = LinkType

  private def getOneValueConstant(value: Parameter): Result[String] =
    value match {
      case OneValueParameter(StringConstant(v)) =>
        Success(v)
      case _ =>
        Failure("""Link requires "label" argument.""")
    }
}

class LinkFormatter extends Formatter {
  override def format(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[List[AgreementElement]] =
    VariableType.convert[LinkInfo](value) map {
      case LinkInfo(labelValue, urlValue) => List(Link(labelValue, urlValue))
    }

  override def missingValueFormat(
      expression: Expression
  ): List[AgreementElement] =
    List(FreeText(Text(s"[[$expression]]")))
  override def stringFormat(
      expression: Expression,
      value: OpenlawValue,
      executionResult: TemplateExecutionResult
  ): Result[String] =
    VariableType.convert[LinkInfo](value) map {
      case LinkInfo(labelValue, urlValue) => s"$labelValue[$urlValue]"
    }
}
