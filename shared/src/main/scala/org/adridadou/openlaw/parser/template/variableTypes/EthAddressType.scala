package org.adridadou.openlaw.parser.template.variableTypes

import java.util

import cats.Eq
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawString, OpenlawValue}
import org.adridadou.openlaw.oracles.UserId
import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress.hex2bytes
import org.adridadou.openlaw.parser.template.{Parameter, TemplateExecutionResult}
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.values.ContractId

case object EthAddressType extends VariableType("EthAddress") {
  override def cast(value: String, executionResult:TemplateExecutionResult): Result[EthereumAddress] = EthereumAddress(value)

  override def internalFormat(value: OpenlawValue): Result[String] = VariableType.convert[EthereumAddress](value).map(_.withLeading0x)

  override def construct(constructorParams: Parameter, executionResult:TemplateExecutionResult): Result[Option[EthereumAddress]] =
    for {
      singleParam <- getSingleParameter(constructorParams)
      value <- singleParam.evaluate(executionResult)
      result <- value match {
        case Some(OpenlawString(value)) => EthereumAddress(value).map(Some(_))
        case Some(value: EthereumAddress) => Success(Some(value))
        case Some(value) => Failure("wrong type " + value.getClass.getSimpleName + ". expecting String")
        case None => Success(None)
      }
    } yield result

  override def getTypeClass: Class[EthereumAddress] = classOf[EthereumAddress]

  def thisType: VariableType = EthAddressType

  def convert(value:OpenlawValue): Result[EthereumAddress] = value match {
    case OpenlawString(strAddr) => EthereumAddress(strAddr)
    case addr:EthereumAddress => Success(addr)
    case _ => VariableType.convert[EthereumAddress](value)
  }
}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
class EthereumAddress(val address: Array[Byte]) extends OpenlawNativeValue {

  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(address)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumAddress(otherAddress) => util.Arrays.equals(otherAddress, address)
    case _ => false
  }
}

object EthereumAddress {
  implicit val ethereumAddressEnc: Encoder[EthereumAddress] = deriveEncoder
  implicit val ethereumAddressDec: Decoder[EthereumAddress] = deriveDecoder

  val maxAddressSize = 20
  val maxSignatureSize = 63

  def unapply(address: EthereumAddress): Option[Array[Byte]] = Some(address.address)

  def apply(address: Array[Byte]): Result[EthereumAddress] = {
    if (address.length > maxAddressSize) {
			Failure("byte array of the address cannot be bigger than 20.value:" + bytes2hex(address))
		} else {
			Success(new EthereumAddress(address))
		}
  }

  def apply(a: String): Result[EthereumAddress] = Option(a) match {
    case None => empty
    case Some(address) if address.startsWith("0x") => apply(address.substring(2))
    case Some(address) if address.length =!= 40 => Failure("the address string should be 40 or 42 with '0x' prefix")
    case Some(address) => apply(hex2bytes(address))
  }

  def empty: Result[EthereumAddress] = EthereumAddress(Array[Byte]())

  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def bytes2hex(bytes: Array[Byte]): String = bytes
    .map(_.toInt & 0xff)
    .map("%02x".format(_)).mkString

  implicit val ethAddressTypeEq:Eq[EthereumAddress] = (x: EthereumAddress, y: EthereumAddress) => x.equals(y)
}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
final case class EthereumSignature(signature: Array[Byte]) extends OpenlawNativeValue {
  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(signature)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumSignature(otherSignature) => util.Arrays.equals(otherSignature, signature)
    case _ => false
  }
}

object EthereumSignature {
  implicit val ethereumSignatureEnc: Encoder[EthereumSignature] = deriveEncoder
  implicit val ethereumSignatureDec: Decoder[EthereumSignature] = deriveDecoder

  implicit val ethereumSignatureEq:Eq[EthereumSignature] = Eq.fromUniversalEquals

  def apply(signature: Array[Byte]): EthereumSignature =
    new EthereumSignature(signature)

  def apply(a: String): Result[EthereumSignature] = Option(a) match {
    case None => Failure("empty signature")
    case Some(address) if address.startsWith("0x") => apply(address.substring(2))
    case Some(address) => Success(apply(hex2bytes(address)))
  }

  def generateSignature(address: String, id: String): Array[Byte] = {
    val combined = address + id
    EthereumSignature(hex2bytes(combined)).signature
  }

  def convert(arg:Any): Result[EthereumSignature] = arg match {
    case s:String => EthereumSignature(s)
    case s:EthereumSignature => Success(s)
    case _ => Failure(s"cannot convert type ${arg.getClass.getSimpleName} to EthereumSignature")
  }
}

object EthereumData {

  implicit val ethereumDataEnc: Encoder[EthereumData] = deriveEncoder
  implicit val ethereumDataDec: Decoder[EthereumData] = deriveDecoder

  implicit val ethereumDataEq:Eq[EthereumData] = Eq.fromUniversalEquals

  val empty:EthereumData = EthereumData(Array[Byte]())

  def apply(data: Array[Byte]): EthereumData =
    new EthereumData(data)

  def apply(a: String): EthereumData = Option(a) match {
    case None => throw new RuntimeException("empty signature")
    case Some(data) if data.startsWith("0x") => apply(data.substring(2))
    case Some(data) => apply(hex2bytes(data))
  }
}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
final case class EthereumData(data: Array[Byte]) extends OpenlawNativeValue {
  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(data)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumData(otherData) => util.Arrays.equals(otherData, data)
    case _ => false
  }

  def merge(data:EthereumData):EthereumData = EthereumData(this.data ++ data.data)
}

object EthereumHash {
  implicit val ethereumHashEnc: Encoder[EthereumHash] = deriveEncoder
  implicit val ethereumHashDec: Decoder[EthereumHash] = deriveDecoder

  implicit val ethereumHashEq:Eq[EthereumHash] = Eq.fromUniversalEquals

  val empty:EthereumHash = EthereumHash(Array[Byte]())

  def apply(hash:String):EthereumHash = new EthereumHash(EthereumAddress.hex2bytes(hash))

}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
final case class EthereumHash(data: Array[Byte]) extends OpenlawNativeValue {
  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(data)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumHash(otherData) => util.Arrays.equals(otherData, data)
    case _ => false
  }
}

final case class OpenlawKeyIdentifier(userId: UserId, contractId: ContractId)
