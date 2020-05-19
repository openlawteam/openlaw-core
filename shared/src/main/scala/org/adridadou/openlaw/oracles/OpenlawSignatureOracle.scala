package org.adridadou.openlaw.oracles

import java.util.UUID

import org.adridadou.openlaw.parser.template.variableTypes._
import cats.implicits._
import cats.Eq
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.values.ContractId
import org.adridadou.openlaw.vm.OpenlawVmEvent

final case class OpenlawSignatureOracle(
    crypto: CryptoService,
    serverAccount: EthereumAddress,
    externalSignatureAccounts: Map[ServiceName, EthereumAddress] = Map()
) {

  def isSignatureValid(
      data: EthereumData,
      signatureEvent: SignatureEvent
  ): Result[Boolean] = signatureEvent match {
    case _: ExternalSignatureEvent => Success(true)

    case event: SignatureEvent =>
      val signedData = EthereumData(crypto.sha256(event.email.email))
        .merge(EthereumData(crypto.sha256(data.data)))

      event.getServiceName
        .map(serviceName => {
          externalSignatureAccounts.get(serviceName) match {
            case Some(account) => Success(account)
            case None          => Failure(s"unknown service ${serviceName.serviceName}")
          }
        })
        .getOrElse(Success(serverAccount))
        .flatMap({ signatureServiceAccount =>
          EthereumAddress(
            crypto
              .validateECSignature(signedData.data, event.signature.signature)
          ).map(derivedAddress => {
            signatureServiceAccount.withLeading0x === derivedAddress.withLeading0x
          })
        })

    case _ => Success(false)
  }
}

trait SignatureEvent extends OpenlawVmEvent {
  def proof: SignatureProof

  def getServiceName: Option[ServiceName]
  def email: Email
  def fullName: String
  def signature: EthereumSignature
}

object ExternalSignatureEvent {
  implicit val externalSignatureEventEnc: Encoder[ExternalSignatureEvent] =
    deriveEncoder
  implicit val externalSignatureEventDec: Decoder[ExternalSignatureEvent] =
    deriveDecoder
}

final case class ExternalSignatureEvent(
    contractId: ContractId,
    email: Email,
    fullName: String,
    serviceName: ServiceName,
    signature: EthereumSignature
) extends SignatureEvent {
  override def getServiceName: Option[ServiceName] = Some(serviceName)
  override def typeIdentifier: String = className[ExternalSignatureEvent]
  override def serialize: String = this.asJson.noSpaces

  override def proof: SignatureProof =
    SignatureProof(contractId, fullName, signature)
}

object OpenlawSignatureEvent {
  implicit val openlawSignatureEventEnc: Encoder[OpenlawSignatureEvent] =
    deriveEncoder
  implicit val openlawSignatureEventDec: Decoder[OpenlawSignatureEvent] =
    deriveDecoder
}

final case class OpenlawSignatureEvent(
    contractId: ContractId,
    email: Email,
    fullName: String,
    signature: EthereumSignature
) extends SignatureEvent {

  override def getServiceName: Option[ServiceName] = None

  override def typeIdentifier: String = className[OpenlawSignatureEvent]
  override def serialize: String = this.asJson.noSpaces

  def proof: SignatureProof =
    SignatureProof(
      contractId = contractId,
      fullName = fullName,
      signature = signature
    )
}

final case class UserId(id: String) {
  override def toString: String = id
}

object UserId {
  implicit val eqForUserIdType: Eq[UserId] = (x: UserId, y: UserId) =>
    x.id === y.id
  implicit val userIdEnc: Encoder[UserId] = deriveEncoder
  implicit val userIdDec: Decoder[UserId] = deriveDecoder

  implicit val userIdKeyEnc: KeyEncoder[UserId] = (key: UserId) => key.id
  implicit val userIdKeyDec: KeyDecoder[UserId] = (key: String) =>
    Some(UserId(key))

  val SYSTEM_ID: UserId = UserId("SYSTEM")

  def generateNew: UserId = UserId(UUID.randomUUID().toString)

}
