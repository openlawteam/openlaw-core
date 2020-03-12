package org.adridadou.openlaw.parser.template

/**
  * Created by davidroon on 05.06.17.
  */
trait CompiledTemplate {
  val block: Block
  val redefinition: VariableRedefinition
  val header: TemplateHeader

  def append(template: CompiledTemplate): CompiledTemplate

  def withRedefinition(redefinition: VariableRedefinition): CompiledTemplate
}
