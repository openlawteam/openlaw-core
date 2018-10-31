package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.{Link, SignatureProof}
import org.adridadou.openlaw.parser.template.variableTypes._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.parser._
import org.adridadou.openlaw.values.ContractId

trait OpenlawIdentityOracle {
  val providerId:String
  def isSignatureValid(data:EthereumData, identifier:IdentityIdentifier, event:SignatureEvent):Boolean
}


object OpenlawSignatureProof {
  implicit val openlawSignatureProofEnc:Encoder[OpenlawSignatureProof] = deriveEncoder[OpenlawSignatureProof]
  implicit val openlawSignatureProofDec:Decoder[OpenlawSignatureProof] = deriveDecoder[OpenlawSignatureProof]
  def deserialize(json:String):Either[Error, OpenlawSignatureProof] = decode[OpenlawSignatureProof](json)
}

case class OpenlawSignatureProof(contractId:ContractId, userId:UserId, fullName:String, address:EthereumAddress, signature:EthereumSignature, txHash:EthereumHash) extends SignatureProof {
  override def serialize: String = this.asJson.noSpaces

  override def validationLink: Link = Link("verify signature",s"/signature/validate?contractId=${contractId.id}&userId=${userId.id}&signature=${signature.toString}")
}