package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}

case class LinkInfo(label: String, url:String) extends OpenlawNativeValue

case object LinkType extends VariableType(name = "Link") {

  private implicit val enc: Encoder[LinkInfo] = deriveEncoder[LinkInfo]
  private implicit val dec: Decoder[LinkInfo] = deriveDecoder[LinkInfo]

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[LinkInfo] = decode[LinkInfo](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] = VariableType.convert[OpenlawString](value)

  override def defaultFormatter: Formatter = new LinkFormatter

  override def getTypeClass: Class[LinkInfo] = classOf[LinkInfo]

  override def checkTypeName(nameToCheck: String): Boolean = Seq("Link").exists(_.equalsIgnoreCase(nameToCheck))

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[LinkInfo]] = {
    constructorParams match {
      case Parameters(seq) =>
        val map = seq.toMap
        (for {
          label <- map.get("label")
          url <- map.get("url")
        } yield for {
          labelValue <- getOneValueConstant(label)
          urlValue <- getOneValueConstant(url)
        } yield LinkInfo(labelValue, urlValue)).sequence
      case _ =>
        Failure("""Link requires parameters, not a unique value or a list""")
    }
  }

  def thisType: VariableType = LinkType

  private def getOneValueConstant(value:Parameter): Result[String] = value match {
    case OneValueParameter(StringConstant(v, _)) =>
      Success(v)
    case _ =>
      Failure("""Link requires "label" argument.""")
  }
}

class LinkFormatter extends Formatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    VariableType.convert[LinkInfo](value) map {
      case LinkInfo(labelValue, urlValue) => Seq(Link(labelValue, urlValue))
    }
}
