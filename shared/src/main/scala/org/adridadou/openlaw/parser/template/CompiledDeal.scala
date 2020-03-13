package org.adridadou.openlaw.parser.template

final case class CompiledDeal(
    header: TemplateHeader,
    block: Block,
    redefinition: VariableRedefinition = VariableRedefinition()
) extends CompiledTemplate {
  override def withRedefinition(
      redefinition: VariableRedefinition
  ): CompiledDeal = this.copy(redefinition = redefinition)

  override def append(template: CompiledTemplate): CompiledTemplate = this.copy(
    header = TemplateHeader(this.header.values ++ template.header.values),
    block = Block(this.block.elems ++ template.block.elems),
    redefinition = VariableRedefinition(
      typeMap = this.redefinition.typeMap ++ template.redefinition.typeMap,
      descriptions = this.redefinition.descriptions ++ template.redefinition.descriptions
    )
  )
}
