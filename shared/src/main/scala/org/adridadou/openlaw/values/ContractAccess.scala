package org.adridadou.openlaw.values

import cats.Eq
import io.circe.{Decoder, Encoder, HCursor, Json}

sealed abstract class ContractAccess(val name:String)

case object ContractSignable extends ContractAccess("signable")
case object ContractNoAccess extends ContractAccess("noaccess")
case object ContractReadonly extends ContractAccess("readonly")
case object ContractEditable extends ContractAccess("editable")

object ContractAccess {

  def apply(name: String): ContractAccess = name match {
    case ContractReadonly.name => ContractReadonly
    case ContractNoAccess.name => ContractNoAccess
    case ContractSignable.name => ContractSignable
    case ContractEditable.name => ContractEditable
  }

  def unapply(arg: ContractAccess): String = arg.name

  implicit val accessDecoder: Decoder[ContractAccess] = (c: HCursor) => {
    for {
      name <- c.as[String]
    } yield ContractAccess(name)
  }
  implicit val accessEncoder: Encoder[ContractAccess] = (a: ContractAccess) => Json.fromString(a.name)
  implicit val eqForContractAccess: Eq[ContractAccess] = Eq.fromUniversalEquals
}