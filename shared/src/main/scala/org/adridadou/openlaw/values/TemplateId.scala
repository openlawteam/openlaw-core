package org.adridadou.openlaw.values

import cats.Eq
import io.circe.{Decoder, Encoder, HCursor, Json, KeyDecoder, KeyEncoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import cats.implicits._
import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress

sealed abstract class TemplateKind(val name: String)
case object Unknown extends TemplateKind("unknown")
case object Agreement extends TemplateKind("agreement")
case object Deal extends TemplateKind("deal")

final case class TemplateId(id: String = "") extends Comparable[TemplateId] {
  override def toString: String = id

  override def compareTo(o: TemplateId): Int = id.compareTo(o.id)
}

object TemplateId {

  def apply(data: Array[Byte]): TemplateId =
    TemplateId(EthereumAddress.bytes2hex(data))

  implicit val templateIdEnc: Encoder[TemplateId] = deriveEncoder
  implicit val templateIdDec: Decoder[TemplateId] = deriveDecoder

  implicit val eq: Eq[TemplateId] = Eq.by(_.id)

}

final case class TemplateIdentifier(title: TemplateTitle, version: Int)

final case class TemplateTitle(originalTitle: String, title: String) {
  override def toString: String = title

  override def equals(obj: Any): Boolean = obj match {
    case other: TemplateTitle => this === other
    case _                    => false
  }

  override def hashCode(): Int = this.title.hashCode
}

object TemplateTitle {

  def apply(): TemplateTitle = TemplateTitle("")
  def apply(title: String): TemplateTitle =
    TemplateTitle(originalTitle = title, title = title.toLowerCase())

  implicit val eq: Eq[TemplateTitle] = (x: TemplateTitle, y: TemplateTitle) =>
    x.title === y.title
  implicit val templateTitleEnc: Encoder[TemplateTitle] = (a: TemplateTitle) =>
    Json.fromString(a.originalTitle)
  implicit val templateTitleDec: Decoder[TemplateTitle] = (c: HCursor) =>
    (for {
      title <- c.downField("title").as[String]
    } yield TemplateTitle(title)) match {
      case Right(title) => Right(title)
      case Left(_) =>
        c.as[String].map(TemplateTitle(_))
    }

  implicit val templateTitleKeyEnc: KeyEncoder[TemplateTitle] =
    (key: TemplateTitle) => key.title
  implicit val templateTitleKeyDec: KeyDecoder[TemplateTitle] = (key: String) =>
    Some(TemplateTitle(key))
}
