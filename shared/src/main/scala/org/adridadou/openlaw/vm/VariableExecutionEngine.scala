package org.adridadou.openlaw.vm

import cats.implicits._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}

trait VariableExecutionEngine {

  private def processNewType(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition
  ): Result[OpenlawExecutionState] = {
    addNewVariable(executionResult, variable)
    Success(executionResult)
  }

  private def processNewVariableInternal(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition,
      executed: Boolean
  ): Result[OpenlawExecutionState] = {
    for {
      _ <- validateType(executionResult, variable)
      currentVariable = if (variable.isAnonymous) {
        variable.copy(name = executionResult.createAnonymousVariable())
      } else {
        variable
      }
      _ <- attempt(addNewVariable(executionResult, currentVariable))
      dependencies <- currentVariable.defaultValue
        .map(_.variables(executionResult))
        .getOrElse(Success(Nil))
      missingVariables = dependencies.filter(v =>
        executionResult.getVariable(v).isEmpty
      )
      result <- if (currentVariable.varType(executionResult) === EthereumEventFilterType) {
        handleMissingVariables(
          missingVariables.filter(_.name =!= "this"),
          executionResult,
          currentVariable,
          executed
        )
      } else {
        handleMissingVariables(
          missingVariables,
          executionResult,
          currentVariable,
          executed
        )
      }
    } yield result
  }

  protected def processNewVariable(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition,
      executed: Boolean
  ): Result[OpenlawExecutionState] =
    for {
      hasCreatedNewType <- registerNewTypeIfNeeded(executionResult, variable)
      result <- if (hasCreatedNewType) {
        processNewType(executionResult, variable)
      } else {
        processNewVariableInternal(executionResult, variable, executed)
      }
    } yield result

  private def handleMissingVariables(
      missingVariables: List[VariableName],
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition,
      executed: Boolean
  ): Result[OpenlawExecutionState] = {
    missingVariables match {
      case Nil =>
        variable.verifyConstructor(executionResult).flatMap { _ =>
          if (executed) {
            executeVariable(executionResult, variable)
          } else {
            Success(executionResult)
          }
        }
      case elem :: Nil =>
        Failure(
          s"""error while processing the new variable ${variable.name}. The variable "${elem.name}" is used in the constructor but has not been defined"""
        )
      case list =>
        Failure(s"error while processing the new variable ${variable.name}. The variables ${list
          .map(v => "\"" + v.name + "\"")
          .mkString(",")} are used in the constructor but have not been defined")
    }
  }

  protected def processDefinedVariable(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition,
      executed: Boolean
  ): Result[OpenlawExecutionState] = {
    if (variable.nameOnly) {
      if (executed) {
        executeVariable(executionResult, variable)
      } else {
        Success(executionResult)
      }
    } else {
      val definedVariable =
        executionResult.getVariable(variable.name).getOrElse(variable)
      validateVariableRedefinition(executionResult, definedVariable, variable) flatMap {
        _ =>
          if (executed) {
            executeVariable(executionResult, variable)
          } else {
            Success(executionResult)
          }
      }
    }
  }

  protected def validateVariableRedefinition(
      executionResult: OpenlawExecutionState,
      definedVariable: VariableDefinition,
      variable: VariableDefinition
  ): Result[VariableDefinition] = {
    if (definedVariable.varType(executionResult) =!= variable.varType(
          executionResult
        )) {
      val title = executionResult
        .getTemplateDefinitionForVariable(definedVariable.name)
        .map(_.name.name.title)

      val currentTitle =
        executionResult.templateDefinition.map(_.name.name.title)
      if (title === currentTitle) {
        Failure(s"type mismatch. ${definedVariable.name} was defined as ${definedVariable
          .varType(executionResult)
          .name} but is now defined as ${variable.varType(executionResult).name}")
      } else {
        Failure(
          s"Variable definition mismatch. variable '${definedVariable.name}' is defined as '${definedVariable
            .varType(executionResult)
            .name}' in '${title.getOrElse("the main template")}' but was '${variable
            .varType(executionResult)
            .name}' in '${currentTitle.getOrElse("the main template")}'"
        )
      }
    } else {
      Success(definedVariable)
    }
  }

  protected def executeVariable(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition
  ): Result[OpenlawExecutionState] = {
    executionResult.executedVariablesInternal append variable.name
    executionResult
      .getVariable(variable.name)
      .map(_.varType(executionResult)) match {
      case Some(TemplateType) =>
        executionResult.executedVariablesInternal append variable.name
        startSubExecution(
          variable,
          executionResult,
          executionType = TemplateExecution
        )
      case Some(ClauseType) =>
        startSubExecution(
          variable,
          executionResult,
          executionType = ClauseExecution
        )
      case _ =>
        val currentVariable =
          executionResult.getVariable(variable.name).getOrElse(variable)
        currentVariable
          .variables(executionResult)
          .map { list =>
            executionResult.executedVariablesInternal appendAll list
            executionResult
          }
    }
  }

  private def startSubExecution(
      variable: VariableDefinition,
      executionResult: OpenlawExecutionState,
      executionType: ExecutionType
  ): Result[OpenlawExecutionState] = {
    variable.evaluate(executionResult) flatMap {
      case Some(definition: TemplateDefinition) =>
        Success(
          executionResult.copy(state = ExecutionWaitForTemplate(
            variable.name,
            definition.name,
            executionType
          )
          )
        )
      case Some(_) =>
        Failure("the variable didn't return a template definition!")
      case None =>
        Failure(s"the template ${variable.name.name} could not be evaluated")
    }
  }

  protected def addNewVariable(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition
  ): Unit =
    executionResult.variablesInternal append redefineDescription(
      executionResult,
      redefineType(executionResult, variable)
    )

  private def redefineType(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition
  ): VariableDefinition =
    executionResult.variableRedefinition.typeMap
      .get(variable.name.name)
      .map(otherType => variable.copy(variableTypeDefinition = Some(otherType)))
      .getOrElse(variable)

  private def redefineDescription(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition
  ): VariableDefinition =
    executionResult.variableRedefinition.descriptions
      .get(variable.name.name)
      .map(description => variable.copy(description = Some(description)))
      .getOrElse(variable)

  protected def processAlias(
      executionResult: OpenlawExecutionState,
      alias: VariableAliasing,
      executed: Boolean
  ): Result[OpenlawExecutionState] = {
    executionResult.getVariable(alias.name) match {
      case Some(variable) if variable.nameOnly =>
        Failure(s"The alias '${alias.name}' was used before being defined.")
      case Some(_) =>
        Failure(
          s"${alias.name} was previously defined as a variable. It cannot be defined as an alias"
        )
      case None =>
        defineAlias(executionResult, alias, executed)
    }
  }

  private def defineAlias(
      executionResult: OpenlawExecutionState,
      alias: VariableAliasing,
      executed: Boolean
  ): Result[OpenlawExecutionState] = {
    executionResult.getAlias(alias.name) match {
      case Some(definedAlias: VariableAliasing) =>
        redefineAlias(executionResult, alias, definedAlias, executed)

      case Some(_) =>
        Failure("that should not happen!")
      case None =>
        defineNewAlias(executionResult, alias, executed)
    }
  }

  private def registerNewTypeIfNeeded(
      executionResult: OpenlawExecutionState,
      variable: VariableDefinition
  ): Result[Boolean] = {
    variable.varType(executionResult) match {
      case ChoiceType =>
        variable.defaultValue.map(param =>
          ChoiceType.construct(param, executionResult)
        ) match {
          case Some(Right(Some(choices))) =>
            executionResult
              .registerNewType(ChoiceType.generateType(variable.name, choices))
              .map(_ => true)
          case Some(Left(ex)) => Failure(ex)
          case _ =>
            Failure(
              s"the new type ${variable.name.name} could not be executed properly"
            )
        }
      case AbstractStructureType =>
        variable.constructT[Structure](executionResult).flatMap {
          case Some(structure) =>
            executionResult
              .registerNewType(
                AbstractStructureType.generateType(variable.name, structure)
              )
              .map(_ => true)
          case None =>
            Failure(
              s"the new type ${variable.name.name} could not be executed properly"
            )
        }
      case AbstractDomainType =>
        variable.constructT[DomainInformation](executionResult).flatMap {
          case Some(domain) =>
            executionResult
              .registerNewType(
                AbstractDomainType.generateType(variable.name, domain)
              )
              .map(_ => true)
          case None =>
            Failure(
              s"the new type ${variable.name.name} could not be executed properly"
            )
        }
      case _ =>
        Success(false)
    }
  }

  private def validateType(
      executionResult: OpenlawExecutionState,
      variableDefinition: VariableDefinition
  ): Result[Unit] =
    variableDefinition.variableTypeDefinition
      .map { typeName =>
        if (executionResult.findVariableType(typeName).isDefined) {
          variableDefinition.validate(executionResult)
        } else {
          Failure(
            s"error while processing the new variable ${variableDefinition.name}. The variable has type ${variableDefinition.variableTypeDefinition.map(_.name).getOrElse("")} but it does not exist"
          )
        }
      }
      .getOrElse(Success.unit)

  protected def redefineAlias(
      executionResult: OpenlawExecutionState,
      alias: VariableAliasing,
      definedAlias: VariableAliasing,
      executed: Boolean
  ): Result[OpenlawExecutionState] =
    for {
      newType <- alias.expressionType(executionResult)
      oldType <- definedAlias.expressionType(executionResult)
      variables <- alias.variables(executionResult)
      result <- {
        if (newType === oldType) {
          executionResult.aliasesInternal.prepend(alias)
          if (executed) {
            executionResult.executedVariablesInternal appendAll variables
          }

          val unknownVariables = variables
            .filter(variable =>
              executionResult.getVariable(variable.name).isEmpty
            )
            .filter(variable => executionResult.getAlias(variable.name).isEmpty)

          if (unknownVariables.isEmpty) {
            alias.validate(executionResult).map { _ =>
              executionResult.aliasesInternal.prepend(alias)
              executionResult
            }
          } else {
            Failure(
              s"alias expression uses undefined variables ${unknownVariables.map(_.name).mkString(",")}"
            )
          }

        } else {
          Failure(
            s"type mismatch. alias type was ${oldType.name} but is now ${newType.name}"
          )
        }
      }
    } yield result

  private def defineNewAlias(
      executionResult: OpenlawExecutionState,
      alias: VariableAliasing,
      executed: Boolean
  ): Result[OpenlawExecutionState] = {
    for {
      variables <- alias.variables(executionResult)
      missingVariables = variables.filter(variable =>
        executionResult.getVariable(variable).isEmpty
      )
      result <- if (missingVariables === Nil) {
        alias.validate(executionResult).flatMap { _ =>
          executionResult.aliasesInternal.prepend(alias)
          alias.expr.variables(executionResult).flatMap { variables =>
            if (executed) {
              executionResult.executedVariablesInternal appendAll variables
            }
            alias.expr.validate(executionResult).map(_ => executionResult)
          }
        }
      } else {
        Failure(
          s"alias expression uses undefined variables ${missingVariables.map(_.name).mkString(",")}"
        )
      }
    } yield result
  }

}
