package org.adridadou.openlaw.parser.contract

import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}

case class ParagraphEdits(edits:Map[Int, String] = Map())

object ParagraphEdits {
  implicit val paragraphEditsEnc:Encoder[ParagraphEdits] = deriveEncoder[ParagraphEdits]
  implicit val paragraphEditsDec:Decoder[ParagraphEdits] = deriveDecoder[ParagraphEdits]
}
