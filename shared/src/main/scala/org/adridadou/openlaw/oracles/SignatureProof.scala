package org.adridadou.openlaw.oracles

import java.time.Instant

import org.adridadou.openlaw.parser.template.Link
import org.adridadou.openlaw.parser.template.variableTypes._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.values.ContractId
import LocalDateTimeHelper._

object SignatureProof {
  implicit val openlawSignatureProofEnc: Encoder[SignatureProof] = deriveEncoder
  implicit val openlawSignatureProofDec: Decoder[SignatureProof] = deriveDecoder
  def deserialize(json: String): Either[Error, SignatureProof] =
    decode[SignatureProof](json)
}

final case class SignatureProof(
    contractId: ContractId,
    fullName: String,
    signature: EthereumSignature,
    signatureDate: Instant
) {
  def serialize: Json = this.asJson
}
