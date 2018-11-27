package org.adridadou.openlaw.vm

import org.adridadou.openlaw.oracles.{CryptoService, OpenlawOracle, OpenlawSignatureOracle}
import org.adridadou.openlaw.parser.template.OpenlawTemplateLanguageParserService
import org.adridadou.openlaw.values.ContractDefinition

class OpenlawVmProvider(cryptoService: CryptoService, parser:OpenlawTemplateLanguageParserService) {

  def create(contractDefinition: ContractDefinition, identityOracle:OpenlawSignatureOracle, executionOrales:Seq[OpenlawOracle[_]]):OpenlawVm =
    OpenlawVm(contractDefinition, cryptoService, parser, identityOracle, executionOrales)
}
