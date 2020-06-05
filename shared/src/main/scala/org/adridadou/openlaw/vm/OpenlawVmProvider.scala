package org.adridadou.openlaw.vm

import org.adridadou.openlaw.oracles.{CryptoService, OpenlawOracle, OpenlawSignatureOracle}
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template.{FormatterDefinition, OpenlawTemplateLanguageParserService, TemplateExecutionResult}
import org.adridadou.openlaw.parser.template.variableTypes.{EthereumAddress, IntegratedServiceDefinition, ServiceName}
import org.adridadou.openlaw.values.ContractDefinition

class OpenlawVmProvider(
    cryptoService: CryptoService,
    parser: OpenlawTemplateLanguageParserService
) {

  def create(
      contractDefinition: ContractDefinition,
      profileAddress: Option[EthereumAddress],
      identityOracle: OpenlawSignatureOracle,
      executionOracles: List[OpenlawOracle[_]],
      externalCallStructures: Map[ServiceName, IntegratedServiceDefinition] =
        Map(),
      overriddenFormatter: (
        Option[FormatterDefinition],
          TemplateExecutionResult
        ) => Option[Formatter] = (_, _) => None
  ): OpenlawVm =
    OpenlawVm(
      contractDefinition,
      profileAddress,
      cryptoService,
      parser,
      identityOracle,
      executionOracles,
      externalCallStructures,
      overriddenFormatter
    )

}
