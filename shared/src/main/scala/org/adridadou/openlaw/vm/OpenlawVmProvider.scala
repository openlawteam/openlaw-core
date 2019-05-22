package org.adridadou.openlaw.vm

import org.adridadou.openlaw.oracles.{CryptoService, OpenlawOracle, OpenlawSignatureOracle}
import org.adridadou.openlaw.parser.template.OpenlawTemplateLanguageParserService
import org.adridadou.openlaw.parser.template.variableTypes.EthereumAddress
import org.adridadou.openlaw.values.ContractDefinition

class OpenlawVmProvider(cryptoService: CryptoService, parser:OpenlawTemplateLanguageParserService) {

  def create(contractDefinition: ContractDefinition, profileAddress:Option[EthereumAddress], identityOracle:OpenlawSignatureOracle, executionOracles:Seq[OpenlawOracle[_]]):OpenlawVm =
    OpenlawVm(contractDefinition, profileAddress, cryptoService, parser, identityOracle, executionOracles)

}
