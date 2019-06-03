package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.decode
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

case class SectionInfo(name: Option[String], numbering: String, value:String) extends OpenlawNativeValue

case object SectionType extends VariableType(name = "Section") with NoShowInForm {

  private implicit val enc: Encoder[SectionInfo] = deriveEncoder[SectionInfo]
  private implicit val dec: Decoder[SectionInfo] = deriveDecoder[SectionInfo]

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[SectionInfo] = handleEither(decode[SectionInfo](value))

  override def internalFormat(value: OpenlawValue): Result[String] = VariableType.convert[OpenlawString](value)

  override def defaultFormatter: Formatter = new SectionFormatter

  override def getTypeClass: Class[SectionInfo] = classOf[SectionInfo]

  // TODO: SectionType is a special type and we should handle it differently. i.e. it shouldn't be possible to use it in the code directly.
  override def checkTypeName(nameToCheck: String): Boolean = Seq("Section").exists(_.equalsIgnoreCase(nameToCheck))

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[SectionInfo]] = {
    constructorParams match {
      case Parameters(seq) =>
        val map = seq.toMap
        Success(for {
          numbering <- map.get("numbering")
          referenceValue <- map.get("reference value")
        } yield SectionInfo(None, getOneValueConstant(numbering), getOneValueConstant(referenceValue)))
      case _ =>
        Failure("""Section requires parameters, not a unique value or a list""")
    }
  }

  def thisType: VariableType = SectionType

  private def getOneValueConstant(value:Parameter):String = value match {
    case OneValueParameter(StringConstant(v, _)) =>
      v
    case _ =>
      throw new RuntimeException("""Section requires "numbering" argument.""")
  }
}

class SectionFormatter extends Formatter {
  override def format(value: OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    VariableType.convert[SectionInfo](value) map {
      case SectionInfo(_, _, referenceValue) => Seq(FreeText(Text(referenceValue)))
    }
}

