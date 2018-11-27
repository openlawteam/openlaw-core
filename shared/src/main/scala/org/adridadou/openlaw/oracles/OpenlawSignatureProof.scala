package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.{Link, SignatureProof}
import org.adridadou.openlaw.parser.template.variableTypes._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.values.ContractId

object OpenlawSignatureProof {
  implicit val openlawSignatureProofEnc:Encoder[OpenlawSignatureProof] = deriveEncoder[OpenlawSignatureProof]
  implicit val openlawSignatureProofDec:Decoder[OpenlawSignatureProof] = deriveDecoder[OpenlawSignatureProof]
  def deserialize(json:String):Either[Error, OpenlawSignatureProof] = decode[OpenlawSignatureProof](json)
}

case class OpenlawSignatureProof(contractId:ContractId, fullName:String, signature:EthereumSignature, txHash:EthereumHash) extends SignatureProof {
  override def serialize: String = this.asJson.noSpaces

  override def validationLink: Link = Link("verify signature",s"/signature/validate?contractId=${contractId.id}&signature=${signature.toString}")
}