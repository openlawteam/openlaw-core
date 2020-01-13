package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.{Link, SignatureProof}
import org.adridadou.openlaw.parser.template.variableTypes._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.values.ContractId

object OpenlawSignatureProof {
  implicit val openlawSignatureProofEnc:Encoder[OpenlawSignatureProof] = deriveEncoder
  implicit val openlawSignatureProofDec:Decoder[OpenlawSignatureProof] = deriveDecoder
  def deserialize(json:String):Either[Error, OpenlawSignatureProof] = decode[OpenlawSignatureProof](json)
}

object ExternalSignatureProof {
  implicit val openlawSignatureProofEnc:Encoder[ExternalSignatureProof] = deriveEncoder
  implicit val openlawSignatureProofDec:Decoder[ExternalSignatureProof] = deriveDecoder
  def deserialize(json:String):Either[Error, ExternalSignatureProof] = decode[ExternalSignatureProof](json)
}

final case class OpenlawSignatureProof(contractId:ContractId, fullName:String, signature:EthereumSignature, txHash:EthereumHash) extends SignatureProof {
  override def serialize: Json = this.asJson

  override def validationLink: Link = Link("verify signature",s"/web/default/signature/validate?contractId=${contractId.id}&signature=${signature.toString}")
}

final case class ExternalSignatureProof(contractId:ContractId, fullName:String, signature:EthereumSignature) extends SignatureProof {
  override def serialize: Json = this.asJson

  override def validationLink: Link = Link("verify signature",s"/web/default/signature/validate?contractId=${contractId.id}&signature=${signature.toString}")
}
