package org.adridadou.openlaw.oracles

import java.time.LocalDateTime
import java.util.UUID

import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.vm._
import cats.implicits._
import cats.Eq
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.adridadou.openlaw.parser.template.SignatureProof
import org.adridadou.openlaw.values.ContractId

case class OpenlawSignatureOracle(crypto:CryptoService, serverAccount:EthereumAddress) extends OpenlawIdentityOracle {
  override def isSignatureValid(data:EthereumData, identifier: IdentityIdentifier, signatureEvent: SignatureEvent): Boolean = signatureEvent match {
    case event:OpenlawSignatureEvent =>
      val actualAddress = EthereumAddress(crypto.validateECSignature(EthereumData(crypto.sha256(event.email.email))
        .merge(EthereumData(crypto.sha256(data.data))).data, event.signature.signature))

      actualAddress === serverAccount

    case _ => false
  }

  override val providerId: String = "openlaw"
}

trait SignatureEvent extends OpenlawVmEvent {
  def fullName: String
  def proof:SignatureProof
  val userId:UserId
}

object OpenlawSignatureEvent {
  implicit val openlawSignatureEventEnc: Encoder[OpenlawSignatureEvent] = deriveEncoder[OpenlawSignatureEvent]
  implicit val openlawSignatureEventDec: Decoder[OpenlawSignatureEvent] = deriveDecoder[OpenlawSignatureEvent]
}

case class OpenlawSignatureEvent(contractId:ContractId, userId:UserId, email:Email, fullName:String, signature: EthereumSignature, ethereumHash:EthereumHash) extends SignatureEvent {
  override def typeIdentifier: String = className[OpenlawSignatureEvent]
  override def serialize: String = this.asJson.noSpaces

  override def proof: OpenlawSignatureProof = OpenlawSignatureProof(
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

  val SYSTEM_ID: UserId = UserId("SYSTEM")

  def generateNew:UserId = UserId(UUID.randomUUID().toString)

}

case class OpenlawUser(userId:UserId, token: Option[String],
                       tokenValidityDate: Option[LocalDateTime])