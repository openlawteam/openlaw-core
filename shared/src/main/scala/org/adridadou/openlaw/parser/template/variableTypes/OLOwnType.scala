package org.adridadou.openlaw.parser.template.variableTypes

import java.time.LocalDateTime

import cats.implicits._
import org.adridadou.openlaw.parser.template.{
  TemplateExecutionResult,
  VariableMemberKey,
  VariableName
}
import org.adridadou.openlaw.values.ContractId
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}
import LocalDateTimeHelper._

case object OLOwnType extends VariableType("OLInfo") with NoShowInForm {

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OLInformation] =
    decode[OLInformation](value).leftMap(FailureException(_))

  override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType.convert[OLInformation](value).map(_.asJson.noSpaces)

  override def thisType: VariableType = OLOwnType
  override def getTypeClass: Class[OLInformation] = classOf[OLInformation]

  override def keysType(
      keys: List[VariableMemberKey],
      expr: Expression,
      executionResult: TemplateExecutionResult
  ): Result[VariableType] = keys match {
    case Nil => Success(OLOwnType)
    case VariableMemberKey(Left(VariableName("now"))) :: Nil =>
      Success(DateTimeType)
    case VariableMemberKey(Left(VariableName("creationDate"))) :: Nil =>
      Success(DateTimeType)
    case _ :: Nil => Success(TextType)
    case _ =>
      Failure(
        s"Openlaw contract info has only one level of properties. invalid property access ${keys.mkString(".")}"
      )
  }

  override def access(
      value: OpenlawValue,
      name: VariableName,
      keys: List[VariableMemberKey],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] = {
    keys match {
      case Nil => Success(Some(executionResult.info))
      case VariableMemberKey(Left(VariableName(head))) :: Nil =>
        accessProperty(executionResult.info, head).map(Some(_))
      case _ =>
        Failure(
          s"Openlaw contract info has only one level of properties. invalid property access ${keys.mkString(".")}"
        )
    }
  }

  override def validateKeys(
      name: VariableName,
      keys: List[VariableMemberKey],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] = keys match {
    case Nil => Success.unit
    case VariableMemberKey(Left(VariableName(head))) :: Nil =>
      checkProperty(head)
    case _ => Failure(s"invalid property ${keys.mkString(".")}")
  }

  private def accessProperty(
      info: OLInformation,
      property: String
  ): Result[String] =
    property.toLowerCase().trim match {
      case "id" => Success(info.id.map(_.id).getOrElse("-"))
      case "profileaddress" =>
        Success(info.profileAddress.map(_.withLeading0x).getOrElse("-"))
      case "now" =>
        DateTimeType.internalFormat(info.now)
      case _ => Failure(s"property '$property' not found for type 'OLInfo'")
    }

  private def checkProperty(key: String): Result[Unit] =
    accessProperty(
      OLInformation(
        id = None,
        profileAddress = None,
        creationDate = None,
        now = LocalDateTime.now()
      ),
      key
    ) match {
      case Left(ex) => Failure(ex)
      case Right(_) => Success(())
    }
}

object OLInformation {
  implicit val olInformationEnc: Encoder[OLInformation] = deriveEncoder
  implicit val olInformationDec: Decoder[OLInformation] = deriveDecoder
}

final case class OLInformation(
    id: Option[ContractId] = None,
    creationDate: Option[LocalDateTime] = None,
    profileAddress: Option[EthereumAddress] = None,
    now: LocalDateTime = LocalDateTime.now()
) extends OpenlawNativeValue
