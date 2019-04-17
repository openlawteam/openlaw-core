package org.adridadou.openlaw.parser.template.variableTypes
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.result.{Failure, Result, Success}

object AddressType extends VariableType(name = "Address") {

  override def cast(value: String, executionResult: TemplateExecutionResult): Address =
    cast(value)

  def cast(value: String): Address = decode[Address](value) match {
      case Right(address) => address
      case Left(ex) => throw new RuntimeException(ex)
    }

  override def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[Address]] =
    Failure("Address type does not support constructor")

  override def defaultFormatter: Formatter = AddressFormatter

  override def internalFormat(value: Any): String = VariableType.convert[Address](value).asJson.noSpaces

  override def keysType(keys: Seq[String], expr: Expression, executionResult: TemplateExecutionResult): Result[VariableType] = keys.toList match {
    case _::tail if tail.isEmpty => Success(TextType)
    case _::_ => Failure(s"Address has only one level of properties. invalid property access ${keys.mkString(".")}")
    case _ => Success(AddressType)
  }

  override def access(value: Any, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Any] = {
    keys.toList match {
      case head::tail if tail.isEmpty => accessProperty(getAddress(value, executionResult), head)
      case _::_ => Failure(s"Address has only one level of properties. invalid property access ${keys.mkString(".")}")
      case _ => Success(value)
    }
  }

  override def validateKeys(name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Unit] = keys.toList match {
    case Nil => Success(())
    case head::tail if tail.isEmpty => checkProperty(head)
    case _::_ => Failure(s"invalid property ${keys.mkString(".")}")
  }

  override def getTypeClass: Class[_ <: Address] = classOf[Address]

  def thisType: VariableType = AddressType

  private def getAddress(value:Any, executionResult: TemplateExecutionResult):Address = value match {
    case json: String => cast(json, executionResult)
    case address:Address => address
    case _ => throw new RuntimeException(s"invalid type ${value.getClass.getSimpleName} for Address")
  }

  private def checkProperty(key:String): Result[Unit] = accessProperty(Address(), key) match {
    case Left(ex) => Failure(ex)
    case Right(_) => Success(())
  }

  private def accessProperty(address: Address, property: String): Result[String] = {
    property.toLowerCase() match {
      case "placeid" => Success(address.placeId)
      case "streetname" => Success(address.streetName)
      case "street name" => Success(address.streetName)
      case "streetnumber" => Success(address.streetNumber)
      case "street number" => Success(address.streetNumber)
      case "city" => Success(address.city)
      case "state" => Success(address.state)
      case "country" => Success(address.country)
      case "zipcode" => Success(address.zipCode)
      case "zip" => Success(address.zipCode)
      case _ => Failure(s"property '$property' not found for type Address")
    }
  }
}

case class Address(placeId:String = "", streetName:String = "", streetNumber:String = "", city:String = "", state:String = "", country:String = "", zipCode:String = "", formattedAddress:String = "")

object Address{
  implicit val addressEnc: Encoder[Address] = deriveEncoder[Address]
  implicit val addressDec: Decoder[Address] = deriveDecoder[Address]
}

object AddressFormatter extends Formatter {
  override def format(value: Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] = value match {
    case address:Address => Success(Seq(FreeText(Text(address.formattedAddress))))
    case _ => Failure(s"incompatible type. Expecting address, got ${value.getClass.getSimpleName}")
  }
}