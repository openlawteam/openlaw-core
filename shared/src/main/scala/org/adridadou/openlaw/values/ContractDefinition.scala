package org.adridadou.openlaw.values

import java.time.Instant

import org.adridadou.openlaw.oracles.{CryptoService, UserId}
import org.adridadou.openlaw.parser.template.variableTypes.{
  EthereumData,
  EthereumHash,
  TemplateSourceIdentifier
}

final case class ContractDefinition(
    creatorId: UserId = UserId.SYSTEM_ID,
    mainTemplate: TemplateId,
    templates: Map[TemplateSourceIdentifier, TemplateId] = Map.empty,
    parameters: TemplateParameters,
    creationDate: Instant = Instant.now(),
    // If the contract is instead formed by a PDF file rather than an openlaw template, we include the hash of the PDF
    // file
    pdfHash: Option[EthereumHash] = None
) {

  def id(crypto: CryptoService): ContractId =
    ContractId(EthereumData(crypto.sha256(idString)).toString)

  def idString: String =
    creatorId.id + "#" +
      mainTemplate.id + "#" +
      paramsToChecksum(parameters) + "#" +
      templatesKey + "#" +
      addPdfHash()

  private def templatesKey: String =
    templates
      .map({ case (definition, id) => definition.name + "_" + id.id })
      .toSeq
      .sorted
      .mkString("#")

  private def addPdfHash(): String = {
    pdfHash match {
      case Some(hash) => "#" + hash.toString
      case None       => ""
    }
  }

  private def paramsToChecksum(parameters: TemplateParameters): String =
    parameters.params.toSeq
      .sortBy({ case (key, _) => key.name })
      .map({ case (key, value) => s"${key.name}||$value" })
      .mkString("#")
}

final case class TemplateScope(
    parameters: TemplateParameters = TemplateParameters()
)
