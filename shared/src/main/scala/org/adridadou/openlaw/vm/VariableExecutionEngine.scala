package org.adridadou.openlaw.vm

import org.adridadou.openlaw.parser.template._
import cats.implicits._
import org.adridadou.openlaw.parser.template.variableTypes.{AbstractStructureType, ChoiceType, TemplateDefinition, TemplateType}

import scala.util.{Failure, Success, Try}

trait VariableExecutionEngine {

  protected def processNewVariable(executionResult: TemplateExecutionResult, variable:VariableDefinition, executed:Boolean):Either[String, TemplateExecutionResult] = {
    registerNewTypeIfNeeded(executionResult, variable) flatMap {
      case true =>
        addNewVariable(executionResult, variable)
        Right(executionResult)
      case false =>
        validateType(executionResult, variable) match {
          case Some(error) => Left(error)
          case None =>
            val currentVariable = if (variable.isAnonymous) {
              variable.copy(name = executionResult.createAnonymousVariable())
            } else {
              variable
            }

            addNewVariable(executionResult, currentVariable)
            currentVariable.defaultValue.map(_.variables(executionResult)).getOrElse(Seq())
              .filter(newVariable => executionResult.getVariable(newVariable.name).isEmpty).toList match {
              case Nil =>
                Try(variable.verifyConstructor(executionResult)) match {
                  case Success(_) =>
                    if (executed) {
                      executeVariable(executionResult, currentVariable)
                    } else {
                      Right(executionResult)
                    }
                  case Failure(ex) =>
                    Left(ex.getMessage)
                }
              case list =>
                Left(s"error while processing the new variable ${variable.name}. The variables ${list.map(_.name).mkString(",")} are used in the constructor but have not been defined")
            }
        }
    }
  }

  protected def processDefinedVariable(executionResult: TemplateExecutionResult, variable: VariableDefinition, executed: Boolean):Either[String, TemplateExecutionResult] = {
    if(variable.nameOnly) {
      if(executed) {
        executeVariable(executionResult, variable)
      } else {
        Right(executionResult)
      }
    } else {
      val definedVariable = executionResult.getVariable(variable.name).getOrElse(variable)
      validateVariableRedefinition(executionResult, definedVariable, variable) flatMap { _ =>
        if(executed) {
          executeVariable(executionResult, variable)
        } else {
          Right(executionResult)
        }
      }
    }
  }

  protected def validateVariableRedefinition(executionResult: TemplateExecutionResult, definedVariable: VariableDefinition, variable: VariableDefinition):Either[String, VariableDefinition] = {
    if (definedVariable.varType(executionResult) =!= variable.varType(executionResult)) {
      val title = executionResult.getTemplateDefinitionForVariable(definedVariable.name)
        .map(_.name.name.title)

      val currentTitle = executionResult.templateDefinition.map(_.name.name.title)
      if(title === currentTitle) {
        Left(s"type mismatch. ${definedVariable.name} was defined as ${definedVariable.varType(executionResult).name} but is now defined as ${variable.varType(executionResult).name}")
      } else {
        Left(s"Variable definition mismatch. variable ${definedVariable.name} is defined as ${definedVariable.varType(executionResult).name} in ${title.getOrElse("the main template")} but was ${variable.varType(executionResult).name} in ${currentTitle.getOrElse("the main template")}")
      }
    } else {
      Right(definedVariable)
    }
  }

  protected def executeVariable(executionResult: TemplateExecutionResult, variable:VariableDefinition):Either[String, TemplateExecutionResult] = {
    executionResult.executedVariables append variable.name
    executionResult.getVariable(variable.name).map(_.varType(executionResult)) match {
      case Some(TemplateType) =>
        startSubExecution(variable, executionResult)
      case _ =>
        val currentVariable = executionResult.getVariable(variable.name).getOrElse(variable)
        executionResult.executedVariables appendAll currentVariable.defaultValue
          .map(_.variables(executionResult)).getOrElse(Seq())
        Right(executionResult)
    }
  }

  private def startSubExecution(variable: VariableDefinition, executionResult: TemplateExecutionResult):Either[String, TemplateExecutionResult] = {
    variable.evaluate(executionResult) match {
      case Some(definition:TemplateDefinition) =>
        Right(executionResult.copy(state = ExecutionWaitForTemplate(variable.name, definition.name)))
      case Some(_) =>
        Left("the variable didn't return a template definition!")
      case None =>
        Left(s"the template ${variable.name.name} could not be evaluated")
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

  protected def processAlias(executionResult: TemplateExecutionResult, alias: VariableAliasing, executed:Boolean): Either[String, TemplateExecutionResult] = {
    executionResult.getVariable(alias.name) match {
      case Some(variable) if variable.nameOnly =>
        Left(s"The alias '${alias.name}' was used before being defined.")
      case Some(_) =>
        Left(s"${alias.name} was previously defined as a variable. It cannot be defined as an alias")
      case None =>
        defineAlias(executionResult, alias, executed)
    }
  }

  private def defineAlias(executionResult: TemplateExecutionResult, alias:VariableAliasing, executed:Boolean):Either[String, TemplateExecutionResult] = {
    executionResult.getAlias(alias.name) match {
      case Some(definedAlias:VariableAliasing) =>
        redefineAlias(executionResult, alias, definedAlias, executed)

      case Some(_) =>
        Left("that should not happen!")
      case None =>
        defineNewAlias(executionResult, alias, executed)
    }
  }

  private def registerNewTypeIfNeeded(executionResult: TemplateExecutionResult, variable:VariableDefinition):Either[String, Boolean] = {
    variable.varType(executionResult) match {
      case ChoiceType =>
        variable.defaultValue.flatMap(ChoiceType.construct(_, executionResult)) match {
          case Some(choices) =>
            executionResult.registerNewType(ChoiceType.generateType(variable.name, choices)).map(_ => true)
          case None =>
            Left(s"the new type ${variable.name.name} could not be executed properly")
        }
      case AbstractStructureType =>
        variable.defaultValue.flatMap(AbstractStructureType.construct(_, executionResult)) match {
          case Some(structure) =>
            executionResult.registerNewType(AbstractStructureType.generateType(variable.name, structure)).map(_ => true)
          case None =>
            Left(s"the new type ${variable.name.name} could not be executed properly")
        }
      case _ =>
        Right(false)
    }
  }

  private def validateType(executionResult: TemplateExecutionResult, variableDefinition: VariableDefinition): Option[String] = {
    variableDefinition.variableTypeDefinition.flatMap { typeName =>
      if (executionResult.findVariableType(typeName).isDefined) {
        variableDefinition.validate(executionResult)
      } else {
        Some(s"error while processing the new variable ${variableDefinition.name}. The variable has type ${variableDefinition.variableTypeDefinition.map(_.name).getOrElse("")} but it does not exist")
      }
    }
  }

  protected def redefineAlias(executionResult: TemplateExecutionResult, alias: VariableAliasing, definedAlias: VariableAliasing, executed: Boolean):Either[String, TemplateExecutionResult] = {
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
        alias.validate(executionResult) match {
          case Some(err) =>
            Left(err)
          case None =>
            executionResult.aliases.prepend(alias)
            Right(executionResult)
        }

      } else {
        Left(s"alias expression uses undefined variables ${unknownVariables.map(_.name).mkString(",")}")
      }

    } else {
      Left(s"type mismatch. alias type was ${oldType.name} but is now ${newType.name}")
    }
  }

  private def defineNewAlias(executionResult: TemplateExecutionResult, alias:VariableAliasing, executed:Boolean):Either[String, TemplateExecutionResult] = {
    alias.variables(executionResult)
      .filter(variable => executionResult.getVariable(variable).isEmpty)
      .filter(variable => executionResult.getAlias(variable).isEmpty).toList match {
      case Nil =>
        alias.validate(executionResult) match {
          case Some(err) =>
            Left(err)
          case None =>
            executionResult.aliases.prepend(alias)
            if(executed) {
              executionResult.executedVariables appendAll alias.expr.variables(executionResult)
            }
            alias.expr.validate(executionResult) match {
              case Some(err) => Left(err)
              case None => Right(executionResult)
            }
        }
      case variables =>
        Left(s"alias expression uses undefined variables ${variables.map(_.name).mkString(",")}")
    }
  }

}
