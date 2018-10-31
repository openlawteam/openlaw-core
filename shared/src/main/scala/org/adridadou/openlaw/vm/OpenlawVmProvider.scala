package org.adridadou.openlaw.vm

import org.adridadou.openlaw.oracles.{CryptoService, OpenlawIdentityOracle, OpenlawOracle}
import org.adridadou.openlaw.parser.template.OpenlawTemplateLanguageParserService
import org.adridadou.openlaw.values.ContractDefinition

class OpenlawVmProvider(cryptoService: CryptoService, parser:OpenlawTemplateLanguageParserService) {

  def create(contractDefinition: ContractDefinition, identityOracles:Seq[OpenlawIdentityOracle], executionOrales:Seq[OpenlawOracle[_]]):OpenlawVm =
    OpenlawVm(contractDefinition, cryptoService, parser, identityOracles.map(oracle => oracle.providerId -> oracle).toMap, executionOrales)
}
