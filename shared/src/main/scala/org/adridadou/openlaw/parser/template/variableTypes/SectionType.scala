package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}

final case class SectionInfo(name: Option[String], numbering: String, value:String) extends OpenlawNativeValue

case object SectionType extends VariableType(name = "Section") with NoShowInFormButRender {

  private implicit val enc: Encoder[SectionInfo] = deriveEncoder
  private implicit val dec: Decoder[SectionInfo] = deriveDecoder

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[SectionInfo] = decode[SectionInfo](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] = VariableType.convert[OpenlawString](value)

  override def defaultFormatter: Formatter = new SectionFormatter

  override def getTypeClass: Class[SectionInfo] = classOf[SectionInfo]

  // TODO: SectionType is a special type and we should handle it differently. i.e. it shouldn't be possible to use it in the code directly.
  override def checkTypeName(nameToCheck: String): Boolean = Seq("Section").exists(_.equalsIgnoreCase(nameToCheck))

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[SectionInfo]] = {
    constructorParams match {
      case Parameters(seq) =>
        val map = seq.toMap
        (for {
          numbering <- map.get("numbering")
          reference <- map.get("reference value")
        } yield for {
          numberingValue <- getOneValueConstant(numbering)
          referenceValue <- getOneValueConstant(reference)
        } yield SectionInfo(None, numberingValue, referenceValue)).sequence
      case _ =>
        Failure("""Section requires parameters, not a unique value or a list""")
    }
  }

  def thisType: VariableType = SectionType

  private def getOneValueConstant(value:Parameter): Result[String] = value match {
    case OneValueParameter(StringConstant(v, _)) =>
      Success(v)
    case _ =>
      Failure("""Section requires "numbering" argument.""")
  }
}

class SectionFormatter extends Formatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[List[AgreementElement]] =
    VariableType.convert[SectionInfo](value) map {
      case SectionInfo(_, _, referenceValue) => List(FreeText(Text(referenceValue)))
    }
}

