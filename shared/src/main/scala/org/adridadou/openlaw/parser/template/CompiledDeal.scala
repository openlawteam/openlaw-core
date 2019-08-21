package org.adridadou.openlaw.parser.template

import java.time.Clock

final case class CompiledDeal(
                         header:TemplateHeader,
                         block:Block,
                         redefinition:VariableRedefinition = VariableRedefinition(),
                         clock:Clock = Clock.systemDefaultZone
                       ) extends CompiledTemplate {
  override def withRedefinition(redefinition: VariableRedefinition): CompiledDeal = this.copy(redefinition = redefinition)
}
