package org.adridadou.openlaw.parser.abi

import enumeratum.{Enum, EnumEntry}
import enumeratum.EnumEntry.Lowercase
import org.adridadou.openlaw.result.{FailureException, Result}
import io.circe.parser
import io.circe.generic.auto._
import org.adridadou.openlaw.parser.template.variableTypes._

final case class AbiParam(
    indexed: Option[Boolean],
    name: String,
    `type`: String
) {
  def isArray: Boolean = `type`.contains("[")
  def isDynamic: Boolean =
    `type`.contains("[]") || `type` == "string" || `type` == "bytes"
  override def toString: String =
    "AbiParam{" + "indexed=" + indexed + ", name='" + name + '\'' + ", type='" + `type` + '\'' + '}'
}

final case class AbiEntry(
    name: Option[String],
    anonymous: Option[Boolean],
    constant: Option[Boolean],
    payable: Option[Boolean],
    stateMutability: Option[String],
    signature: Option[String],
    inputs: Option[List[AbiParam]],
    outputs: Option[List[AbiParam]],
    `type`: String
)

object AbiParser {

  sealed abstract class AbiType(
      val scalaType: Class[_],
      val openlawType: VariableType,
      val dynamic: Boolean = true
  ) extends EnumEntry
      with Lowercase

  object AbiType extends Enum[AbiType] {
    val values = findValues

    case object Uint extends AbiType(classOf[Int], NumberType)
    case object Uint8 extends AbiType(classOf[Int], NumberType)
    case object Uint16 extends AbiType(classOf[Int], NumberType)
    case object Uint32 extends AbiType(classOf[Int], NumberType)
    case object Uint64 extends AbiType(classOf[Int], NumberType)
    case object Uint128 extends AbiType(classOf[Long], NumberType)
    case object Uint256 extends AbiType(classOf[Long], NumberType)
    case object Int extends AbiType(classOf[Int], NumberType)
    case object Int8 extends AbiType(classOf[Int], NumberType)
    case object Int16 extends AbiType(classOf[Int], NumberType)
    case object Int32 extends AbiType(classOf[Int], NumberType)
    case object Int64 extends AbiType(classOf[Long], NumberType)
    case object Int128 extends AbiType(classOf[Long], NumberType)
    case object Int256 extends AbiType(classOf[Long], NumberType)
    case object Bool extends AbiType(classOf[Boolean], YesNoType)
    case object String extends AbiType(classOf[String], TextType, true)
    case object Address
        extends AbiType(classOf[EthereumAddress], EthAddressType)

    /* Not supported for now
    case object Array extends AbiType
    case object Bytes extends AbiType
    case object Bytes32 extends AbiType
    case object Bytes16 extends AbiType
    case object Bytes8 extends AbiType
   */
  }

  def parse(json: String): Result[List[AbiEntry]] =
    for {
      parsed <- parser.parse(json).left.map(FailureException(_))
      decoded <- parsed.as[List[AbiEntry]].left.map(FailureException(_))
    } yield decoded
}
