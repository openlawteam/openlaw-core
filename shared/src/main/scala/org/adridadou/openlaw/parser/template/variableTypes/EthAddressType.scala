package org.adridadou.openlaw.parser.template.variableTypes

import java.util

import cats.Eq
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.adridadou.openlaw.oracles.UserId
import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress.hex2bytes
import org.adridadou.openlaw.parser.template.{Parameter, TemplateExecutionResult}
import org.adridadou.openlaw.result.{attempt, Failure, Result, Success}
import org.adridadou.openlaw.values.ContractId

import scala.util.Try

case object EthAddressType extends VariableType("EthAddress") {
  override def cast(value: String, executionResult:TemplateExecutionResult): Result[EthereumAddress] = Success(EthereumAddress(value))

  override def internalFormat(value: Any): Result[String] = VariableType.convert[EthereumAddress](value).map(_.withLeading0x)

  override def construct(constructorParams: Parameter, executionResult:TemplateExecutionResult): Result[Option[EthereumAddress]] = {
    getSingleParameter(constructorParams).evaluate(executionResult).sequence.flatMap { option =>
      option match {
        case Some(value: String) => attempt(Some(EthereumAddress(value)))
        case Some(value: EthereumAddress) => Success(Some(value))
        case None => Success(None)
        case value => Failure("wrong type " + value.getClass.getSimpleName + ". expecting String")
      }
    }
  }

  override def getTypeClass: Class[EthereumAddress] = classOf[EthereumAddress]

  def thisType: VariableType = EthAddressType
}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
case class EthereumAddress(address: Array[Byte]) {

  if (address.length > EthereumAddress.maxAddressSize) throw new RuntimeException("byte array of the address cannot be bigger than 20.value:" + EthereumAddress.bytes2hex(address))

  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(address)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumAddress(otherAddress) => util.Arrays.equals(otherAddress, address)
    case _ => false
  }
}

object EthereumAddress {
  implicit val ethereumAddressEnc: Encoder[EthereumAddress] = deriveEncoder[EthereumAddress]
  implicit val ethereumAddressDec: Decoder[EthereumAddress] = deriveDecoder[EthereumAddress]

  val maxAddressSize = 20
  val maxSignatureSize = 63

  def apply(address: Array[Byte]): EthereumAddress = {
    if (address.length > maxAddressSize) throw new RuntimeException("byte array of the address cannot be bigger than 20.value:" + bytes2hex(address))
    new EthereumAddress(address)
  }

  def apply(a: String): EthereumAddress = Option(a) match {
    case None => empty
    case Some(address) if address.startsWith("0x") => apply(address.substring(2))
    case Some(address) if address.length =!= 40 => throw new RuntimeException("the address string should be 40 or 42 with '0x' prefix")
    case Some(address) => apply(hex2bytes(address))
  }

  def empty: EthereumAddress = EthereumAddress(Array[Byte]())

  def hex2bytes(hex: String): Array[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
  }

  def bytes2hex(bytes: Array[Byte]): String = bytes
    .map(_.toInt & 0xff)
    .map("%02x".format(_)).mkString

  implicit val ethAddressTypeEq:Eq[EthereumAddress] = (x: EthereumAddress, y: EthereumAddress) => x.equals(y)
}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
case class EthereumSignature(signature: Array[Byte]) {
  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(signature)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumSignature(otherSignature) => util.Arrays.equals(otherSignature, signature)
    case _ => false
  }
}

object EthereumSignature {
  implicit val ethereumSignatureEnc: Encoder[EthereumSignature] = deriveEncoder[EthereumSignature]
  implicit val ethereumSignatureDec: Decoder[EthereumSignature] = deriveDecoder[EthereumSignature]

  implicit val ethereumSignatureEq:Eq[EthereumSignature] = Eq.fromUniversalEquals

  def apply(signature: Array[Byte]): EthereumSignature =
    new EthereumSignature(signature)

  def apply(a: String): EthereumSignature = Option(a) match {
    case None => throw new RuntimeException("empty signature")
    case Some(address) if address.startsWith("0x") => apply(address.substring(2))
    case Some(address) => apply(hex2bytes(address))
  }

  def generateSignature(address: String, id: String): Array[Byte] = {
    val combined = address + id
    EthereumSignature(hex2bytes(combined)).signature
  }
}

object EthereumData {

  implicit val ethereumDataEnc: Encoder[EthereumData] = deriveEncoder[EthereumData]
  implicit val ethereumDataDec: Decoder[EthereumData] = deriveDecoder[EthereumData]

  implicit val ethereumDataEq:Eq[EthereumData] = Eq.fromUniversalEquals

  val empty:EthereumData = EthereumData(Array[Byte]())

  def apply(signature: Array[Byte]): EthereumData =
    new EthereumData(signature)

  def apply(a: String): EthereumData = Option(a) match {
    case None => throw new RuntimeException("empty signature")
    case Some(data) if data.startsWith("0x") => apply(data.substring(2))
    case Some(data) => apply(hex2bytes(data))
  }
}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
case class EthereumData(data: Array[Byte]) {
  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(data)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumData(otherData) => util.Arrays.equals(otherData, data)
    case _ => false
  }

  def merge(data:EthereumData):EthereumData = EthereumData(this.data ++ data.data)
}

object EthereumHash {
  implicit val ethereumHashEnc: Encoder[EthereumHash] = deriveEncoder[EthereumHash]
  implicit val ethereumHashDec: Decoder[EthereumHash] = deriveDecoder[EthereumHash]

  implicit val ethereumHashEq:Eq[EthereumHash] = Eq.fromUniversalEquals

  val empty:EthereumHash = EthereumHash(Array[Byte]())

}

@SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
case class EthereumHash(data: Array[Byte]) {
  def withLeading0x: String = "0x" + this.toString
  override def toString: String = EthereumAddress.bytes2hex(data)

  override def equals(obj: scala.Any): Boolean = obj match {
    case EthereumHash(otherData) => util.Arrays.equals(otherData, data)
    case _ => false
  }
}

case class OpenlawKeyIdentifier(userId: UserId, contractId: ContractId)
