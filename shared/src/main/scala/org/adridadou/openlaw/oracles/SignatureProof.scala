package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.Link
import org.adridadou.openlaw.parser.template.variableTypes._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.values.ContractId

object SignatureProof {
  implicit val openlawSignatureProofEnc:Encoder[SignatureProof] = deriveEncoder
  implicit val openlawSignatureProofDec:Decoder[SignatureProof] = deriveDecoder
  def deserialize(json:String):Either[Error, SignatureProof] = decode[SignatureProof](json)
}


final case class SignatureProof(contractId:ContractId, fullName:String, signature:EthereumSignature) {
  def serialize: Json = this.asJson

  def validationLink: Link = Link("verify signature",s"/web/default/signature/validate?contractId=${contractId.id}&signature=${signature.toString}")
}
