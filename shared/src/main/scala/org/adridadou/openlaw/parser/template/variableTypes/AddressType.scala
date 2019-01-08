package org.adridadou.openlaw.parser.template.variableTypes
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.formatters.Formatter

object AddressType extends VariableType(name = "Address") {

  override def cast(value: String, executionResult: TemplateExecutionResult): Address =
    cast(value)

  def cast(value: String): Address = decode[Address](value) match {
      case Right(address) => address
      case Left(ex) => throw new RuntimeException(ex)
    }

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Option[Any] =
    throw new RuntimeException("Address type does not support constructor")

  override def defaultFormatter: Formatter = AddressFormatter

  override def internalFormat(value: Any): String = VariableType.convert[Address](value).asJson.noSpaces

  override def keysType(keys: Seq[String], executionResult: TemplateExecutionResult): VariableType = keys.toList match {
    case _::tail if tail.isEmpty => TextType
    case _::_ => throw new RuntimeException(s"Address has only one level of properties. invalid property access ${keys.mkString(".")}")
    case _ => AddressType
  }

  override def access(value: Any, keys: Seq[String], executionResult: TemplateExecutionResult): Either[String, Any] = {
    keys.toList match {
      case head::tail if tail.isEmpty => accessProperty(getAddress(value, executionResult), head)
      case _::_ => Left(s"Address has only one level of properties. invalid property access ${keys.mkString(".")}")
      case _ => Right(value)
    }
  }

  override def validateKeys(name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Option[String] = keys.toList match {
    case Nil => None
    case head::tail if tail.isEmpty => checkProperty(head)
    case _::_ => Some(s"invalid property ${keys.mkString(".")}")
  }

  override def getTypeClass: Class[_ <: Address] = classOf[Address]

  def thisType: VariableType = AddressType

  private def getAddress(value:Any, executionResult: TemplateExecutionResult):Address = value match {
    case json: String => cast(json, executionResult)
    case address:Address => address
    case _ => throw new RuntimeException(s"invalid type ${value.getClass.getSimpleName} for Address")
  }

  private def checkProperty(key:String):Option[String] = accessProperty(Address(), key) match {
    case Left(ex) => Some(ex)
    case Right(_) => None
  }

  private def accessProperty(address: Address, property: String):Either[String, String] = {
    property.toLowerCase() match {
      case "placeid" => Right(address.placeId)
      case "streetname" => Right(address.streetName)
      case "street name" => Right(address.streetName)
      case "streetnumber" => Right(address.streetNumber)
      case "street number" => Right(address.streetNumber)
      case "city" => Right(address.city)
      case "state" => Right(address.state)
      case "country" => Right(address.country)
      case "zipcode" => Right(address.zipCode)
      case "zip" => Right(address.zipCode)
      case _ => Left(s"property '$property' not found for type Address")
    }
  }
}

case class Address(placeId:String = "", streetName:String = "", streetNumber:String = "", city:String = "", state:String = "", country:String = "", zipCode:String = "", formattedAddress:String = "")

object Address{
  implicit val addressEnc: Encoder[Address] = deriveEncoder[Address]
  implicit val addressDec: Decoder[Address] = deriveDecoder[Address]
}

object AddressFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Either[String, Seq[AgreementElement]] = value match {
    case address:Address => Right(Seq(FreeText(Text(address.formattedAddress))))
    case _ =>
      Left(s"incompatible type. Expecting address, got ${value.getClass.getSimpleName}")
  }
}