package org.adridadou.openlaw.vm

import cats.implicits._
import org.adridadou.openlaw.result.Implicits.RichEither
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Failure, handleFatalErrors, Result, Success}

trait VariableExecutionEngine {

  protected def processNewVariable(executionResult: TemplateExecutionResult, variable:VariableDefinition, executed:Boolean): Result[TemplateExecutionResult] = {
    registerNewTypeIfNeeded(executionResult, variable).flatMap {
      case true =>
        addNewVariable(executionResult, variable)
        Success(executionResult)
      case false =>
        validateType(executionResult, variable).flatMap { _ =>
          val currentVariable = if (variable.isAnonymous) {
            variable.copy(name = executionResult.createAnonymousVariable())
          } else {
            variable
          }

          addNewVariable(executionResult, currentVariable)
          currentVariable.defaultValue.map(_.variables(executionResult)).getOrElse(Seq())
            .filter(newVariable => executionResult.getVariable(newVariable.name).isEmpty).toList match {
            case Nil =>
              variable.verifyConstructor(executionResult).left.flatMap(handleFatalErrors).flatMap { _ =>
                if (executed) {
                  executeVariable(executionResult, currentVariable)
                } else {
                  Success(executionResult)
                }
              }
            case list =>
              Failure(s"error while processing the new variable ${variable.name}. The variables ${list.map(v => "\"" + v.name + "\"").mkString(",")} are used in the constructor but have not been defined")
          }
        }
    }
  }

  protected def processDefinedVariable(executionResult: TemplateExecutionResult, variable: VariableDefinition, executed: Boolean): Result[TemplateExecutionResult] = {
    if(variable.nameOnly) {
      if(executed) {
        executeVariable(executionResult, variable)
      } else {
        Success(executionResult)
      }
    } else {
      val definedVariable = executionResult.getVariable(variable.name).getOrElse(variable)
      validateVariableRedefinition(executionResult, definedVariable, variable) flatMap { _ =>
        if(executed) {
          executeVariable(executionResult, variable)
        } else {
          Success(executionResult)
        }
      }
    }
  }

  protected def validateVariableRedefinition(executionResult: TemplateExecutionResult, definedVariable: VariableDefinition, variable: VariableDefinition): Result[VariableDefinition] = {
    if (definedVariable.varType(executionResult) =!= variable.varType(executionResult)) {
      val title = executionResult.getTemplateDefinitionForVariable(definedVariable.name)
        .map(_.name.name.title)

      val currentTitle = executionResult.templateDefinition.map(_.name.name.title)
      if(title === currentTitle) {
        Failure(s"type mismatch. ${definedVariable.name} was defined as ${definedVariable.varType(executionResult).name} but is now defined as ${variable.varType(executionResult).name}")
      } else {
        Failure(s"Variable definition mismatch. variable ${definedVariable.name} is defined as ${definedVariable.varType(executionResult).name} in ${title.getOrElse("the main template")} but was ${variable.varType(executionResult).name} in ${currentTitle.getOrElse("the main template")}")
      }
    } else {
      Success(definedVariable)
    }
  }

  protected def executeVariable(executionResult: TemplateExecutionResult, variable:VariableDefinition): Result[TemplateExecutionResult] = {
    executionResult.executedVariables append variable.name
    executionResult.getVariable(variable.name).map(_.varType(executionResult)) match {
      case Some(TemplateType) =>
        startSubExecution(variable, executionResult)
      case _ =>
        val currentVariable = executionResult.getVariable(variable.name).getOrElse(variable)
        executionResult.executedVariables appendAll currentVariable.defaultValue
          .map(_.variables(executionResult)).getOrElse(Seq())
        Success(executionResult)
    }
  }

  private def startSubExecution(variable: VariableDefinition, executionResult: TemplateExecutionResult): Result[TemplateExecutionResult] = {
    variable.evaluate(executionResult) match {
      case Some(definition:TemplateDefinition) =>
        Success(executionResult.copy(state = ExecutionWaitForTemplate(variable.name, definition.name)))
      case Some(_) =>
        Failure("the variable didn't return a template definition!")
      case None =>
        Failure(s"the template ${variable.name.name} could not be evaluated")
    }
  }

  protected def addNewVariable(executionResult: TemplateExecutionResult, variable:VariableDefinition):Unit =
    executionResult.variables append redefineDescription(executionResult, redefineType(executionResult, variable))

  private def redefineType(executionResult: TemplateExecutionResult, variable:VariableDefinition):VariableDefinition =
    executionResult
      .variableRedefinition.typeMap.get(variable.name.name)
      .map(otherType => variable.copy(variableTypeDefinition = Some(otherType)))
      .getOrElse(variable)

  private def redefineDescription(executionResult: TemplateExecutionResult, variable:VariableDefinition):VariableDefinition =
    executionResult.variableRedefinition.descriptions
      .get(variable.name.name).map(description => variable.copy(description = Some(description)))
      .getOrElse(variable)

  protected def processAlias(executionResult: TemplateExecutionResult, alias: VariableAliasing, executed:Boolean): Result[TemplateExecutionResult] = {
    executionResult.getVariable(alias.name) match {
      case Some(variable) if variable.nameOnly =>
        Failure(s"The alias '${alias.name}' was used before being defined.")
      case Some(_) =>
        Failure(s"${alias.name} was previously defined as a variable. It cannot be defined as an alias")
      case None =>
        defineAlias(executionResult, alias, executed)
    }
  }

  private def defineAlias(executionResult: TemplateExecutionResult, alias:VariableAliasing, executed:Boolean): Result[TemplateExecutionResult] = {
    executionResult.getAlias(alias.name) match {
      case Some(definedAlias:VariableAliasing) =>
        redefineAlias(executionResult, alias, definedAlias, executed)

      case Some(_) =>
        Failure("that should not happen!")
      case None =>
        defineNewAlias(executionResult, alias, executed)
    }
  }

  private def registerNewTypeIfNeeded(executionResult: TemplateExecutionResult, variable:VariableDefinition): Result[Boolean] = {
    variable.varType(executionResult) match {
      case ChoiceType =>
        variable.defaultValue.map(param => ChoiceType.construct(param, executionResult)) match {
          case Some(Right(Some(choices))) =>
            executionResult.registerNewType(ChoiceType.generateType(variable.name, choices)).map(_ => true)
          case Some(Left(ex)) => handleFatalErrors(ex)
          case _ =>
            Failure(s"the new type ${variable.name.name} could not be executed properly")
        }
      case AbstractStructureType =>
        variable.constructT[Structure](executionResult).left.flatMap(handleFatalErrors).flatMap {
          case Some(structure) =>
            executionResult.registerNewType(AbstractStructureType.generateType(variable.name, structure)).map(_ => true)
          case None =>
            Failure(s"the new type ${variable.name.name} could not be executed properly")
        }
      case _ =>
        Success(false)
    }
  }

  private def validateType(executionResult: TemplateExecutionResult, variableDefinition: VariableDefinition): Result[Unit] =
    variableDefinition.variableTypeDefinition.map { typeName =>
      if (executionResult.findVariableType(typeName).isDefined) {
        variableDefinition.validate(executionResult)
      } else {
        Failure(s"error while processing the new variable ${variableDefinition.name}. The variable has type ${variableDefinition.variableTypeDefinition.map(_.name).getOrElse("")} but it does not exist")
      }
    }.getOrElse(Success(()))

  protected def redefineAlias(executionResult: TemplateExecutionResult, alias: VariableAliasing, definedAlias: VariableAliasing, executed: Boolean): Result[TemplateExecutionResult] = {
    val newType = alias.expressionType(executionResult)
    val oldType = definedAlias.expressionType(executionResult)

    if(newType === oldType) {
      executionResult.aliases.prepend(alias)
      if(executed) {
        executionResult.executedVariables appendAll alias.variables(executionResult)
      }
      val unknownVariables = alias
        .variables(executionResult)
        .filter(variable => executionResult.getVariable(variable.name).isEmpty)
        .filter(variable => executionResult.getAlias(variable.name).isEmpty)

      if(unknownVariables.isEmpty) {
        alias.validate(executionResult).map { _ =>
          executionResult.aliases.prepend(alias)
          executionResult
        }
      } else {
        Failure(s"alias expression uses undefined variables ${unknownVariables.map(_.name).mkString(",")}")
      }

    } else {
      Failure(s"type mismatch. alias type was ${oldType.name} but is now ${newType.name}")
    }
  }

  private def defineNewAlias(executionResult: TemplateExecutionResult, alias:VariableAliasing, executed:Boolean): Result[TemplateExecutionResult] = {
    val result = alias.variables(executionResult)
      .filter(variable => executionResult.getVariable(variable).isEmpty)
      .filter(variable => executionResult.getAlias(variable).isEmpty).toList match {
        case Nil =>
          alias.validate(executionResult).flatMap { _ =>
            executionResult.aliases.prepend(alias)
            if (executed) {
              executionResult.executedVariables appendAll alias.expr.variables(executionResult)
            }
            alias.expr.validate(executionResult).map(_ => executionResult)
          }
        case variables =>
          Failure(s"alias expression uses undefined variables ${variables.map(_.name).mkString(",")}")
      }
    result
  }
}
