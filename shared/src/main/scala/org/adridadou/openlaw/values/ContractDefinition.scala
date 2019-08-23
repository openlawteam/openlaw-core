package org.adridadou.openlaw.values

import org.adridadou.openlaw.oracles.{CryptoService, UserId}
import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template.variableTypes.{EthereumData, TemplateSourceIdentifier}


final case class ContractDefinition(
                               creatorId:UserId = UserId.SYSTEM_ID,
                               mainTemplate:TemplateId,
                               templates:Map[TemplateSourceIdentifier, TemplateId] = Map(),
                               parameters:TemplateParameters,
                               paragraphs: Map[Int, ParagraphEdits] = Map()) {

  def id(crypto:CryptoService):ContractId =
    ContractId(EthereumData(crypto.sha256(idString)).toString)

  def idString:String = creatorId.id + "#" +
    mainTemplate.id + "#" +
    paramsToChecksum(parameters) + "#" +
    templatesKey + "#" +
    templateScopeKey

  private def templatesKey:String = templates
    .map({case (definition, id) => definition.name + "_" + id.id}).toSeq.sorted.mkString("#")

  private def templateScopeKey:String = paragraphs.toSeq.sortBy({case (key,_) => key})
    .map({case (index:Int, edits:ParagraphEdits) => s"$index#${editsToChecksum(edits)}"})
    .mkString("#")

  private def paramsToChecksum(parameters:TemplateParameters):String = parameters.params.toSeq.sortBy({case (key,_) => key.name})
    .map({case (key, value) => s"${key.name}||$value"}).mkString("#")

  private def editsToChecksum(edits:ParagraphEdits):String = edits.edits.toSeq.sortBy({case (key,_) => key})
    .map({case (key, value) => s"$key||$value"}).mkString("#")
}

final case class TemplateScope(parameters:TemplateParameters = TemplateParameters(), paragraphEdits: ParagraphEdits = ParagraphEdits())
