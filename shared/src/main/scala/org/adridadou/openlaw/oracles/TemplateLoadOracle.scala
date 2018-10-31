package org.adridadou.openlaw.oracles

import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress
import org.adridadou.openlaw.values.TemplateId
import org.adridadou.openlaw.vm.{LoadTemplateCommand, OpenlawVm, OpenlawVmEvent}
import cats.implicits._
import TemplateId._
import io.circe.{Decoder, Encoder}
import io.circe.syntax._
import io.circe.generic.semiauto._

case class TemplateLoadOracle(crypto:CryptoService) extends OpenlawOracle[LoadTemplate] {
  override def incoming(vm:OpenlawVm, event: LoadTemplate): Either[String, OpenlawVm] = {
    val id = TemplateId(EthereumAddress.bytes2hex(crypto.sha256(event.content)))
    if(vm.contractDefinition.mainTemplate === id) {
      vm(LoadTemplateCommand(id, event))
    } else {
      vm.contractDefinition.templates.values.find(templateId => templateId === id) match {
        case Some(_) =>
          vm(LoadTemplateCommand(id, event))
        case None =>
          Left(s"invalid template for contract ${vm.contractId}")
      }
    }
  }

  override def shouldExecute(event: OpenlawVmEvent): Boolean = event match {
    case _:LoadTemplate => true
    case _ => false
  }
}

object LoadTemplate {
  implicit val loadTemplateEnc: Encoder[LoadTemplate] = deriveEncoder[LoadTemplate]
  implicit val loadTemplateDec: Decoder[LoadTemplate] = deriveDecoder[LoadTemplate]
}

case class LoadTemplate(content:String) extends OpenlawVmEvent {
  override def typeIdentifier: String = className[LoadTemplate]
  override def serialize: String = this.asJson.noSpaces
}