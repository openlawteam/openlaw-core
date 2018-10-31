package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template.VariableName

trait TypeGenerator[T] extends NoShowInForm{
  def generateType(name:VariableName, constructor:T):VariableType
}
