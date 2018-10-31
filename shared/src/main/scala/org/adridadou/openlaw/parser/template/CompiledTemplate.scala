package org.adridadou.openlaw.parser.template

import java.time.Clock

/**
  * Created by davidroon on 05.06.17.
  */

trait CompiledTemplate {
  val block: Block
  val clock: Clock
  val redefinition:VariableRedefinition

  def withRedefinition(redefinition: VariableRedefinition): CompiledTemplate
}
