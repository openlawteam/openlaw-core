package org.adridadou.openlaw.vm

import org.adridadou.ethereum.propeller.values.{EthData, EthSignature}
import org.adridadou.openlaw.oracles.CryptoService

object TestCryptoService extends CryptoService {

  override def sha256(data: Array[Byte]): Array[Byte] = EthData.of(data).sha3().hash
  override def validateECSignature(data: Array[Byte], signature: Array[Byte]): Array[Byte] = EthSignature.of(signature)
    .ecrecover(EthData.of(data)).address

}
