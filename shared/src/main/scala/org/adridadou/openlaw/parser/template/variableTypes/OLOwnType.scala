package org.adridadou.openlaw.parser.template.variableTypes
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.values.ContractId

case object OLOwnType extends VariableType("this") {

  override def cast(value: String, executionResult: TemplateExecutionResult): Any = ???

  override def internalFormat(value: Any): String = ???

  override def thisType: VariableType = OLOwnType
  override def getTypeClass: Class[_] = classOf[OLInformation]
}

case class OLInformation(id:Option[ContractId])