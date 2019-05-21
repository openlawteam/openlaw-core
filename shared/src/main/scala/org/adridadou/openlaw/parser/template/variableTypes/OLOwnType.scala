package org.adridadou.openlaw.parser.template.variableTypes
import org.adridadou.openlaw.parser.template.{TemplateExecutionResult, VariableName}
import org.adridadou.openlaw.values.ContractId
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, Result, Success}

case object OLOwnType extends VariableType("OLInfo") with NoShowInForm {

  override def cast(value: String, executionResult: TemplateExecutionResult): OLInformation = handleEither(decode[OLInformation](value))

  override def internalFormat(value: Any): String = VariableType.convert[OLInformation](value).asJson.noSpaces

  override def thisType: VariableType = OLOwnType
  override def getTypeClass: Class[_] = classOf[OLInformation]

  override def keysType(keys: Seq[String], expr: Expression, executionResult: TemplateExecutionResult): Result[VariableType] = keys.toList match {
    case Nil => Success(OLOwnType)
    case _ :: Nil => Success(TextType)
    case _ => Failure(s"Openlaw contract info has only one level of properties. invalid property access ${keys.mkString(".")}")
  }

  override def access(value: Any, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[Any]] = {
    keys.toList match {
      case Nil => Success(Some(executionResult.info))
      case head::Nil => accessProperty(executionResult.info, head).map(Some(_))
      case _ => Failure(s"Openlaw contract info has only one level of properties. invalid property access ${keys.mkString(".")}")
    }
  }

  override def validateKeys(name:VariableName, keys: Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] = keys.toList match {
    case Nil => Success(())
    case head::Nil => checkProperty(head)
    case _ => Failure(s"invalid property ${keys.mkString(".")}")
  }

  private def accessProperty(info: OLInformation, property: String): Result[String] = {
    property.toLowerCase().trim match {
      case "id" => Success(info.id.map(_.id).getOrElse("-"))
      case "profileAddress" => Success(info.profileAddress.map(_.withLeading0x).getOrElse("-"))
      case _ => Failure(s"property '$property' not found for type 'OLInfo'")
    }
  }

  private def checkProperty(key:String): Result[Unit] = accessProperty(OLInformation(id = None, profileAddress = None), key) match {
    case Left(ex) => Failure(ex)
    case Right(_) => Success(())
  }
}

object OLInformation {
  implicit val olInformationEnc:Encoder[OLInformation] = deriveEncoder[OLInformation]
  implicit val olInformationDec:Decoder[OLInformation] = deriveDecoder[OLInformation]
}

case class OLInformation(id:Option[ContractId], profileAddress:Option[EthereumAddress])