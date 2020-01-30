package org.adridadou.openlaw.oracles

import java.nio.charset.StandardCharsets

trait CryptoService {
  def sha256(data: String): Array[Byte] =
    sha256(data.getBytes(StandardCharsets.UTF_8))
  def sha256(data: Array[Byte]): Array[Byte]
  def validateECSignature(
      data: Array[Byte],
      signature: Array[Byte]
  ): Array[Byte]
}
