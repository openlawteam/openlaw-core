package org.adridadou.openlaw.parser.contract

import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}

final case class ParagraphEdits(edits: Map[Int, String] = Map())

object ParagraphEdits {
  implicit val paragraphEditsEnc: Encoder[ParagraphEdits] = deriveEncoder
  implicit val paragraphEditsDec: Decoder[ParagraphEdits] = deriveDecoder
}
