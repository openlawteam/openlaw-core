package org.adridadou.openlaw

import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers
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

    val Right(ethAddress) = EthereumAddress(arr)
    ethAddress.address shouldEqual arr

    val str1 = ethAddress.toString
    val str2 = ethAddress.withLeading0x

    val Right(address) = EthereumAddress(str1).map(_.address)
		address shouldEqual arr
    val Right(address2) = EthereumAddress(str2).map(_.address)
		address2 shouldEqual arr
    true
  }

}
