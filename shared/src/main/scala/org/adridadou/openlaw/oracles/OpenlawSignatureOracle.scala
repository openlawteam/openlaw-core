package org.adridadou.openlaw.oracles

import java.time.LocalDateTime
import java.util.UUID

import org.adridadou.openlaw.parser.template.variableTypes._
import cats.implicits._
import cats.Eq
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.adridadou.openlaw.values.ContractId
import org.adridadou.openlaw.vm.OpenlawVmEvent

case class OpenlawSignatureOracle(crypto:CryptoService, serverAccount:EthereumAddress) {

  def isSignatureValid(data:EthereumData, signatureEvent: OpenlawSignatureEvent): Boolean = signatureEvent match {
    case event:OpenlawSignatureEvent =>
      val signedData = EthereumData(crypto.sha256(event.email.email))
        .merge(EthereumData(crypto.sha256(data.data)))

      val actualAddress = EthereumAddress(crypto.validateECSignature(signedData.data, event.signature.signature))

      actualAddress === serverAccount

    case _ => false
  }
}

object OpenlawSignatureEvent {
  implicit val openlawSignatureEventEnc: Encoder[OpenlawSignatureEvent] = deriveEncoder[OpenlawSignatureEvent]
  implicit val openlawSignatureEventDec: Decoder[OpenlawSignatureEvent] = deriveDecoder[OpenlawSignatureEvent]
}

case class OpenlawSignatureEvent(contractId:ContractId, email:Email, fullName:String, signature: EthereumSignature, ethereumHash:EthereumHash) extends OpenlawVmEvent {

  override def typeIdentifier: String = className[OpenlawSignatureEvent]
  override def serialize: String = this.asJson.noSpaces

  def proof: OpenlawSignatureProof = OpenlawSignatureProof(
    contractId = contractId,
    fullName = fullName,
    signature = signature,
    txHash = ethereumHash)
}

case class UserId(id:String) {
  override def toString: String = id
}

object UserId {
  implicit val eqForUserIdType: Eq[UserId] = (x: UserId, y: UserId) => x.id === y.id
  implicit val userIdEnc:Encoder[UserId] = deriveEncoder[UserId]
  implicit val userIdDec:Decoder[UserId] = deriveDecoder[UserId]

  implicit val userIdKeyEnc:KeyEncoder[UserId] = (key: UserId) => key.id
  implicit val userIdKeyDec:KeyDecoder[UserId] = (key: String) => Some(UserId(key))

  val SYSTEM_ID: UserId = UserId("SYSTEM")

  def generateNew:UserId = UserId(UUID.randomUUID().toString)

}

case class OpenlawUser(userId:UserId, token: Option[String],
                       tokenValidityDate: Option[LocalDateTime])