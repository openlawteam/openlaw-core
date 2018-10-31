package org.adridadou.openlaw.values

import cats.Eq
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.oracles.CryptoService
import org.adridadou.openlaw.parser.template.variableTypes.{EthereumAddress, EthereumData}

@SerialVersionUID(7843732947346776640L)
case class ContractId(id: String) {
  def stopContract(cryptoService: CryptoService):EthereumData = executionId("_stop_contract", cryptoService)
  def data:EthereumData = EthereumData(id)
  def resumeContract(cryptoService: CryptoService):EthereumData = executionId("_resume_contract", cryptoService)
  def executionId(command:String, crypto:CryptoService):EthereumData = EthereumData(id + EthereumData(crypto.sha256("_" + command)).toString)

  override def toString(): String = id
}

@SerialVersionUID(7843732947346776640L)
object ContractId {

  def apply(data:Array[Byte]):ContractId = ContractId(EthereumAddress.bytes2hex(data))

  implicit val contractIdEnc:Encoder[ContractId] = deriveEncoder[ContractId]
  implicit val contractIdDec:Decoder[ContractId] = deriveDecoder[ContractId]

  implicit val contractEq:Eq[ContractId] = Eq.fromUniversalEquals
}
