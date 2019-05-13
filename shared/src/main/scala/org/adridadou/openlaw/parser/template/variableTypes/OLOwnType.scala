package org.adridadou.openlaw.parser.template.variableTypes
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.values.ContractId
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._

case object OLOwnType extends VariableType("this") with NoShowInForm {

  override def cast(value: String, executionResult: TemplateExecutionResult): OLInformation = handleEither(decode[OLInformation](value))

  override def internalFormat(value: Any): String = VariableType.convert[OLInformation](value).asJson.noSpaces

  override def thisType: VariableType = OLOwnType
  override def getTypeClass: Class[_] = classOf[OLInformation]
}

object OLInformation {
  implicit val olInformationEnc:Encoder[OLInformation] = deriveEncoder[OLInformation]
  implicit val olInformationDec:Decoder[OLInformation] = deriveDecoder[OLInformation]
}

case class OLInformation(id:Option[ContractId])