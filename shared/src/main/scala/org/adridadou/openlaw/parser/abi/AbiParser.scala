package org.adridadou.openlaw.parser.abi

import org.adridadou.openlaw.result.{FailureException, Result}
import io.circe.parser
import io.circe.generic.auto._

case class AbiParam(indexed: Option[Boolean], name: String, `type`: String) {
  def isArray: Boolean = `type`.contains("[")
  def isDynamic: Boolean = `type`.contains("[]") || `type` == "string" || `type` == "bytes"
  override def toString: String = "AbiParam{" + "indexed=" + indexed + ", name='" + name + '\'' + ", type='" + `type` + '\'' + '}'

}

case class AbiEntry (
  anonymous: Option[Boolean],
  constant: Option[Boolean],
  payable : Option[Boolean],
  stateMutability: Option[String],
  name: Option[String],
  signature: Option[String],
  inputs: Option[List[AbiParam]],
  outputs: Option[List[AbiParam]],
  `type`: String
)

object AbiParser {
  def parse(json: String): Result[List[AbiEntry]] =
    for {
      parsed <- parser.parse(json).left.map(FailureException(_))
      decoded <- parsed.as[List[AbiEntry]].left.map(FailureException(_))
    } yield decoded
}
