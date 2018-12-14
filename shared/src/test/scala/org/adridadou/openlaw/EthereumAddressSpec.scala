package org.adridadou.openlaw

import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by davidroon on 09.06.17.
  */
class EthereumAddressSpec extends FlatSpec with Matchers with Checkers {

  "Ethereum address" should "convert from byte array to string and back" in {
    check(forAll(Gen.listOfN(20, arbitrary[Byte]))(checkEncode))
  }

  def checkEncode(lst:List[Byte]):Boolean = {
    val arr = lst.toArray

    val ethAddress = EthereumAddress(arr)
    ethAddress.address shouldEqual arr

    val str1 = ethAddress.toString
    val str2 = ethAddress.withLeading0x

    EthereumAddress(str1).address shouldEqual arr
    EthereumAddress(str2).address shouldEqual arr
    true
  }

}
