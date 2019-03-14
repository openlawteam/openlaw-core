package org.adridadou.openlaw.parser.abi

import org.adridadou.openlaw.result.{FailureException, Result}
import io.circe.parser
import io.circe.generic.auto._

case class AbiParam(indexed: Boolean, name: String, fieldType: String) {
  def isArray: Boolean = fieldType.contains("[")
  def isDynamic: Boolean = fieldType.contains("[]") || fieldType == "string" || fieldType == "bytes"
  override def toString: String = "AbiParam{" + "indexed=" + indexed + ", name='" + name + '\'' + ", type='" + fieldType + '\'' + '}'

}

case class AbiEntry (
  anonymous: Boolean,
  constant: Boolean,
  payable : Boolean,
  stateMutability: String,
  name: String,
  signature: String,
  inputs: List[AbiParam],
  outputs: List[AbiParam],
  fieldType: String
)

object AbiParser {
  def parse(json: String): Result[List[AbiEntry]] = for {
    parsed <- parser.parse(json).left.map(FailureException(_))
    decoded <- parsed.as[List[AbiEntry]].left.map(FailureException(_))
  } yield decoded
}
