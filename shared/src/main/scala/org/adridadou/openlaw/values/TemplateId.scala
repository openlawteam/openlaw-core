package org.adridadou.openlaw.values

import cats.Eq
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import cats.implicits._
import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress

sealed abstract class TemplateKind(val name:String)
case object Unknown extends TemplateKind("unknown")
case object Agreement extends TemplateKind("agreement")
case object Deal extends TemplateKind("deal")

case class TemplateId(id:String = "") extends Comparable[TemplateId] {
  override def toString:String = id

  override def compareTo(o: TemplateId): Int = id.compareTo(o.id)
}

object TemplateId {

  def apply(data:Array[Byte]):TemplateId = TemplateId(EthereumAddress.bytes2hex(data))

  implicit val templateIdEnc:Encoder[TemplateId] = deriveEncoder[TemplateId]
  implicit val templateIdDec:Decoder[TemplateId] = deriveDecoder[TemplateId]

  implicit val eq:Eq[TemplateId] = Eq.by(_.id)

}

case class TemplateIdentifier(title:TemplateTitle, version:Int)
case class TemplateTitle(title:String = "") {
  override def toString:String = title
}

object TemplateTitle {
  implicit val eq:Eq[TemplateTitle] = (x: TemplateTitle, y: TemplateTitle) => x.title === y.title
  implicit val templateTitleEnc:Encoder[TemplateTitle] = deriveEncoder[TemplateTitle]
  implicit val templateTitleDec:Decoder[TemplateTitle] = deriveDecoder[TemplateTitle]
}
