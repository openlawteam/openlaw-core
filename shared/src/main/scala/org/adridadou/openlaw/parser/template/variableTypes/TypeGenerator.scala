package org.adridadou.openlaw.parser.template.variableTypes

import java.util.UUID

import org.adridadou.openlaw.parser.template.VariableName

trait TypeGenerator[T] extends NoShowInForm{
  def generateType(name:VariableName, constructor:T):VariableType
	def generateType(constructor:T):VariableType = generateType(VariableName(UUID.randomUUID().toString), constructor)
}
