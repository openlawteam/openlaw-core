package org.adridadou.openlaw.values

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

final case class InstanceId(id: String)

object InstanceId {
  implicit val instanceIdEnc: Encoder[InstanceId] = deriveEncoder
  implicit val instanceIdDec: Decoder[InstanceId] = deriveDecoder
}
