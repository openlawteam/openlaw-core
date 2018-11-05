package org.adridadou.openlaw.vm

import org.adridadou.openlaw.oracles.CryptoService
import org.adridadou.openlaw.parser.template.variableTypes.{EthereumAddress, EthereumData, EthereumSignature}

object TestCryptoService extends CryptoService {

  override def sha256(data: Array[Byte]): Array[Byte] = data.slice(0, 20)
  override def validateECSignature(data: Array[Byte], signature: Array[Byte]): Array[Byte] =
    signature.toStream.indexOfSlice(data) match {
      case -1 =>
        Array()
      case index =>
        signature.slice(index + data.length, signature.length)
    }
}

case class TestAccount(address:EthereumAddress) {

  def sign(data:EthereumData):EthereumSignature =
    sign(data.data)

  def sign(data:Array[Byte]):EthereumSignature =
    EthereumSignature(data ++ address.address)
}

object TestAccount {
  def newRandom:TestAccount = {
    val address = Array.fill(20)((scala.util.Random.nextInt(256) - 128).toByte)
    TestAccount(EthereumAddress(address))
  }
}


