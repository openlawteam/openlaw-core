package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.{
  Parameter,
  TemplateExecutionResult,
  VariableMemberKey,
  VariableName
}
import org.adridadou.openlaw.parser.template.formatters.{
  Formatter,
  NoopFormatter
}
import org.adridadou.openlaw.result.Result

case object ClauseType extends VariableType("Clause") with NoShowInForm {

  override def getTypeClass: Class[_ <: TemplateDefinition] =
    classOf[TemplateDefinition]

  override def construct(
      constructorParams: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] =
    TemplateType
      .construct(constructorParams, executionResult)

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[TemplateDefinition] = TemplateType.cast(value, executionResult)
  override def internalFormat(value: OpenlawValue): Result[String] =
    TemplateType.internalFormat(value)
  override def defaultFormatter: Formatter = new NoopFormatter

  override def accessVariables(
      name: VariableName,
      keys: List[
        VariableMemberKey
      ],
      executionResult: TemplateExecutionResult
  ): Result[List[
    VariableName
  ]] = TemplateType.accessVariables(name, keys, executionResult)

  override def access(
      value: OpenlawValue,
      variableName: VariableName,
      keys: List[
        VariableMemberKey
      ],
      executionResult: TemplateExecutionResult
  ): Result[Option[
    OpenlawValue
  ]] = TemplateType.access(value, variableName, keys, executionResult)

  override def validateKeys(
      variableName: VariableName,
      keys: List[
        VariableMemberKey
      ],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] =
    TemplateType.validateKeys(variableName, keys, expression, executionResult)

  override def keysType(
      keys: List[
        VariableMemberKey
      ],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[
    VariableType
  ] = TemplateType.keysType(keys, expression, executionResult)

  def thisType: VariableType = ClauseType
}
