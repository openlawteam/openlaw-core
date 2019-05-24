package org.adridadou.openlaw.vm

import java.util.concurrent.atomic.AtomicInteger

import org.adridadou.openlaw.result._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.{ContractId, TemplateParameters}
import cats.implicits._
import org.adridadou.openlaw.oracles.OpenlawSignatureProof
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.printers.SectionHelper

import scala.annotation.tailrec
import scala.collection.mutable

class OpenlawExecutionEngine extends VariableExecutionEngine {

  /**
    * Entry point for simple agreements
    */
  def execute(mainTemplate:CompiledTemplate, parameters:TemplateParameters):Result[OpenlawExecutionState] =
    execute(mainTemplate, parameters, Map())

  /**
    * Entry point. This is where you start the execution of the main template
    */
  def execute(mainTemplate:CompiledTemplate, parameters:TemplateParameters, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[OpenlawExecutionState] =
    execute(mainTemplate, parameters, templates, Map(), Map(), None)

  def execute(mainTemplate:CompiledTemplate, parameters:TemplateParameters, templates:Map[TemplateSourceIdentifier, CompiledTemplate], signatureProofs:Map[Email, OpenlawSignatureProof], executions:Map[VariableName, Executions], id:Option[ContractId]):Result[OpenlawExecutionState] = {
    val executionResult = OpenlawExecutionState(
      parameters = parameters,
      id = TemplateExecutionResultId(s"@@anonymous_main_template_id@@"),
      info = OLInformation(id = id),
      template = mainTemplate,
      executions = executions,
      anonymousVariableCounter = new AtomicInteger(0),
      embedded = false,
      variableRedefinition = mainTemplate.redefinition,
      remainingElements = mutable.Buffer(mainTemplate.block.elems:_*),
      clock = mainTemplate.clock,
      signatureProofs = signatureProofs
    )

    resumeExecution(executionResult, templates).flatMap(newResult => {
      mainTemplate match {
        case agreement:CompiledAgreement if newResult.agreements.isEmpty =>
          attempt(executionResult.structuredMainTemplate(agreement))
            .map { t =>
              newResult.agreementsInternal.append(t)
              newResult
            }
        case _ =>
          Success(newResult)
      }
    })
  }

  /**
    * This method is used if the execution stops due to a missing template and you want to resume the execution
    */
  @tailrec
  final def resumeExecution(executionResult: OpenlawExecutionState, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[OpenlawExecutionState] = {
    executionResult.state match {
      case ExecutionFinished =>
        executionResult.parentExecution match {
          case Some(_) =>
            // has to be in a matcher for tail call optimization
            attempt(finishExecution(executionResult, templates)).flatten match {
              case Success(result) => resumeExecution(result, templates)
              case f => f
            }
          case None =>
            Success(executionResult)
        }

      case ExecutionWaitForTemplate(variableName, identifier, willBeUsedForEmbedded) =>
        templates.get(identifier) match {
          case Some(template) =>
            // has to be in a matcher for tail call optimization
            attempt(executionResult.startSubExecution(variableName, template, willBeUsedForEmbedded)).flatten match {
              case Success(result) => resumeExecution(result, templates)
              case f@Failure(_,_) => f
            }
          case None => Success(executionResult)
        }

      case ExecutionReady =>
        // has to be in a matcher for tail call optimization
        executeInternal(executionResult, templates) match {
          case Success(result) => resumeExecution(result, templates)
          case f => f
        }
      case ExecutionFailed(Failure(ex, message)) =>
        Failure(ex, message)
    }
  }

  private def finishExecution(executionResult: OpenlawExecutionState, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[OpenlawExecutionState] = {
    executionResult.parentExecutionInternal.map(parent => {
      (for {
        definition <- executionResult.templateDefinition
      } yield {
        if(!executionResult.embedded) {
          executionResult.template match {
            case agreement:CompiledAgreement =>
              attempt(getRoot(parent).agreementsInternal.append(executionResult.structuredInternal(agreement)))
            case _ =>
          }
        }

        validateSubExecution(executionResult, definition).map(_ => {
          if(executionResult.embedded) {
            parent.finishedEmbeddedExecutions append executionResult
          }
          parent
        })

      }).getOrElse(Success(parent)) map (_.copy(state = ExecutionReady))

    }).getOrElse(Success(executionResult))
  }

  private final def executeInternal(execution: OpenlawExecutionState, templates:Map[TemplateSourceIdentifier, CompiledTemplate]): Result[OpenlawExecutionState] = {
    execution.forEachExecutions.headOption match {
      case Some(embeddedExecution) =>
        execution.forEachExecutions.remove(0)
        Success(embeddedExecution)
      case _ =>
        execution.remainingElements.headOption match {
          case Some(elem) =>
            execution.remainingElements.remove(0)
            processExecutedElement(execution, elem, templates)
          case None =>
            Success(execution.copy(state = ExecutionFinished))
        }
    }
  }

  private def processVariableMember(executionResult: OpenlawExecutionState, variableMember: VariableMember, executed:Boolean):Result[OpenlawExecutionState] = {
    processVariable(executionResult, VariableDefinition(name = variableMember.name), executed)
    variableMember.validate(executionResult).map(_ => executionResult)
  }

  private def processExecutedElement(executionResult: OpenlawExecutionState, element: TemplatePart, templates:Map[TemplateSourceIdentifier, CompiledTemplate]): Result[OpenlawExecutionState] = element match {
    case variable: VariableDefinition =>
      processVariable(executionResult, variable, executed = true)

    case variableMember: VariableMember =>
      processVariableMember(executionResult, variableMember, executed = true)

    case variable:VariableName =>
      processVariable(executionResult, VariableDefinition(name = variable.name), executed = true)

    case alias:VariableAliasing =>
      processAlias(executionResult, alias, executed = true)

    case TemplateText(elems) =>
      executionResult.remainingElements.prependAll(elems)
      Success(executionResult)

    case ConditionalBlock(subBlock, elseSubBlock, expr) =>
      executeConditionalBlock(executionResult, templates, subBlock, elseSubBlock, expr)
    case foreachBlock:ForEachBlock =>
      executeForEachBlock(executionResult, foreachBlock)

    case Table(header, rows) =>
      val initialValue:Result[OpenlawExecutionState] = Success(executionResult)
      (header.flatten ++ rows.flatten.flatten)
        .foldLeft(initialValue)((exec, elem) => exec.flatMap(processExecutedElement(_, elem, templates)))

    case ConditionalBlockSet(blocks) =>
      executeConditionalBlockSet(executionResult, blocks)

    case CodeBlock(elems) =>
      val initialValue:Result[OpenlawExecutionState] = Success(executionResult)
      elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))

    case section:VariableSection =>
      val initialValue:Result[OpenlawExecutionState] = Success(executionResult)
      section.variables
        .foldLeft(initialValue)((exec, variableDefinition) => exec.flatMap(processVariable(_, variableDefinition, executed = true)))
        .flatMap(processVariableSection(_, section))

    case section:Section =>
      executionResult.sectionLevelStack append section.lvl
      processSection(section, executionResult)

    case _ =>
      Success(executionResult)
  }

  private def generateFullSectionValue(section:Section, sectionValue:String, executionResult: OpenlawExecutionState):String = {
    val fullSectionValue = (1 to section.lvl).map({
      case section.lvl => sectionValue
      case idx => executionResult.getLastSectionByLevel(idx)
    }).mkString(".")
    fullSectionValue
  }

  private def processSection(section: Section, executionResult: OpenlawExecutionState): Result[OpenlawExecutionState] = {
    (section.definition.flatMap(_.parameters).flatMap(_.parameterMap.toMap.get("numbering")).flatMap({
      case OneValueParameter(expr) => expr.evaluate(executionResult)
      case _ => None
    }) match {
      case Some(value:BigDecimal) =>
        val values = (0 until value.toInt).map(_ => section.lvl)
        val allValues = ((1 until section.lvl)
          .flatMap(lvl => {
            (0 until SectionHelper.calculateNumberInList(lvl, executionResult.allSectionLevelStack)).map(_ => lvl)
          }) ++ values).toList

        Success(values,  -1 :: allValues)
      case Some(badValue) =>
        Failure(s"numbering parameter in section definition should be a number, not ${badValue.getClass.getSimpleName}")
      case None =>
        Success(executionResult.sectionLevelStack, Nil)
    }).map({ case (numbering, newSectionValues) =>
      val overrideSymbol = section.overrideSymbol(executionResult)
      val overrideFormat = section.overrideFormat(executionResult)
      val sectionValue = SectionHelper.generateListNumber(section.lvl, numbering, overrideSymbol, overrideFormat)
      val referenceValue = SectionHelper.generateReferenceValue(section.lvl, numbering, overrideSymbol)
      val params = Seq(
        "reference value" -> OneValueParameter(StringConstant(generateFullSectionValue(section, referenceValue, executionResult))),
        "numbering" -> OneValueParameter(StringConstant(sectionValue))
      )
      val name = section.definition
        .map(_.name)
        .filter(_ =!= "_") //_ means anonymous
        .map(VariableName(_))
        .getOrElse(executionResult.createAnonymousVariable())

      val variable = VariableDefinition(name = name, variableTypeDefinition = Some(VariableTypeDefinition("Section")), defaultValue = Some(Parameters(params)))
      executionResult.variablesInternal.append(variable)
      executionResult.executedVariablesInternal.append(name)
      executionResult.sectionNameMapping put (section.uuid , name)
      executionResult.sectionNameMappingInverseInternal put (name, section.uuid)
      executionResult.addLastSectionByLevel(section.lvl, referenceValue)
      executionResult.addSectionLevelStack(newSectionValues)
      executionResult.addProcessedSection(section, SectionHelper.calculateNumberInList(section.lvl, numbering))

      executionResult
    })
  }

  private def executeForEachBlock(executionResult: OpenlawExecutionState, foreachBlock: ForEachBlock): Result[OpenlawExecutionState] = {
    foreachBlock.toCompiledTemplate(executionResult).flatMap({ case (template, expressionType) =>
      val initialValue: Result[OpenlawExecutionState] = Success(executionResult)
      executionResult.executedVariablesInternal appendAll foreachBlock.expression.variables(executionResult)

      val elements = foreachBlock.expression.evaluate(executionResult)
        .map(value => VariableType.convert[CollectionValue](value).list)
        .getOrElse(Seq())

      elements.foldLeft(initialValue)((eitherExecutionResult, element) => eitherExecutionResult.flatMap(_ => {
        val anonymousVariable = executionResult.createAnonymousVariable()
        executionResult.variablesInternal append VariableDefinition(name = anonymousVariable, variableTypeDefinition = Some(VariableTypeDefinition(TemplateType.name)), defaultValue = Some(OneValueParameter(StringConstant(anonymousVariable.name))))
        executionResult.executedVariablesInternal append anonymousVariable

        executionResult.startForEachExecution(anonymousVariable, template, foreachBlock.variable, element, expressionType).map(_ => executionResult)
      })
      )
    })
  }

  private def executeConditionalBlockSet(executionResult: OpenlawExecutionState, blocks: Seq[ConditionalBlock]):Result[OpenlawExecutionState] = {
    blocks.foreach({
      subBlock =>
        subBlock.conditionalExpression match {
          case variable: VariableDefinition =>
            processVariable(executionResult, variable, executed = true)
          case _ =>
            executionResult.executedVariablesInternal appendAll subBlock.conditionalExpression.variables(executionResult)
        }
    })

    blocks.find(_.conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean]))
      .map(subBlock => {
        executionResult.remainingElements.prependAll(subBlock.block.elems)
        Success(executionResult)
      }).getOrElse(Success(executionResult))
  }

  private def getRoot(parent:OpenlawExecutionState):OpenlawExecutionState = parent.parentExecutionInternal match {
    case Some(parentExecution) => getRoot(parentExecution)
    case None => parent
  }

  private def validateSubExecution(result: OpenlawExecutionState, templateDefinition: TemplateDefinition): Result[OpenlawExecutionState] = {
    val initialValue:Result[OpenlawExecutionState] = Success(result)
    result.getVariables
      .foldLeft(initialValue)((value, variable) =>
        value.flatMap(_ => validateSubExecution(result, templateDefinition, variable))
      )
  }

  private def validateSubExecution(result:OpenlawExecutionState, currentTemplateDefinition:TemplateDefinition, variable:VariableDefinition): Result[OpenlawExecutionState] = {
    val otherType = result.getVariable(variable.name).map(_.varType(result)).getOrElse(variable.varType(result))
    if(otherType =!= variable.varType(result)) {
      val typeString = variable.variableTypeDefinition.map(_.name).getOrElse("<undefined>")
      Failure(s"Variable definition mismatch. variable ${variable.name} is defined as $typeString in ${currentTemplateDefinition.name.name} but was ${otherType.name} in ${result.templateDefinition.map(_.name.name.title).getOrElse("the main template")}")
    } else {
      result.parentExecutionInternal.map(parent => validateSubExecution(parent, currentTemplateDefinition, variable)).getOrElse(Success(result))
    }
  }

  private def processVariableSection(executionResult: OpenlawExecutionState, section: VariableSection):Result[OpenlawExecutionState] = {
    val existingSection = executionResult.variableSectionsInternal.getOrElse(section.name, collection.mutable.Buffer[VariableName]())
    existingSection.appendAll(section.variables.map(_.name))

    if(!executionResult.variableSectionsInternal.contains(section.name)) {
      executionResult.variableSectionListInternal.append(section.name)
    }

    executionResult.variableSectionsInternal.put(section.name, existingSection)
    Success(executionResult)
  }

  private def processCodeElement(executionResult: OpenlawExecutionState, templates:Map[TemplateSourceIdentifier, CompiledTemplate], element:TemplatePart): Result[OpenlawExecutionState] = element match {
    case variable: VariableDefinition =>
      processVariable(executionResult, variable, executed = false)

    case variable:VariableName =>
      processVariable(executionResult, VariableDefinition(name = variable.name), executed = false)

    case alias:VariableAliasing =>
      processAlias(executionResult, alias, executed = false)

    case ConditionalBlock(subBlock, elseSubBlock, expr) =>
      processConditionalBlock(executionResult, subBlock, elseSubBlock, expr, templates)

    case ConditionalBlockSet(blocks) =>
      processConditionalBlockSet(executionResult, blocks)

    case section:VariableSection =>
      val initialValue:Result[OpenlawExecutionState] = Success(executionResult)
      section.variables.foldLeft(initialValue)((exec, variableDefinition) => exec.flatMap(e => processVariable(e, variableDefinition, executed = false)))
      processVariableSection(executionResult, section)

    case _ =>
      Success(executionResult)
  }

  private def processConditionalBlockSet(executionResult: OpenlawExecutionState, blocks: Seq[ConditionalBlock]):Result[OpenlawExecutionState] = {
    blocks.foreach({
      subBlock =>
        subBlock.conditionalExpression match {
          case variable: VariableDefinition =>
            processVariable(executionResult, variable, executed = false)
          case _ =>
        }
    })

    blocks.find(_.conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean])) match {
      case Some(subBlock) =>
        executionResult.remainingElements.prependAll(subBlock.block.elems)
      case None =>
    }

    Success(executionResult)
  }

  private def processConditionalBlock(executionResult: OpenlawExecutionState, block: Block, elseBlock:Option[Block], expression: Expression, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[OpenlawExecutionState] = {
    expression match {
      case variable:VariableDefinition =>
        processVariable(executionResult, variable, executed = false)
      case _ =>
    }
    if(expression.evaluate(executionResult).exists(VariableType.convert[Boolean])) {
      val initialValue:Result[OpenlawExecutionState] = Success(executionResult)
      val initialValue2 = block.elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))
      elseBlock.map(_.elems.foldLeft(initialValue2)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))).getOrElse(initialValue2)
    } else {
      Success(executionResult)
    }
  }

  private def processVariable(executionResult: OpenlawExecutionState, variable: VariableDefinition, executed:Boolean): Result[OpenlawExecutionState] = {
    executionResult.getExpression(variable.name) match {
      case Some(_:VariableDefinition) =>
        processDefinedVariable(executionResult, variable, executed)
      case Some(alias:VariableAliasing) if variable.nameOnly =>
        executionResult.executedVariablesInternal appendAll alias.expr.variables(executionResult)
        Success(executionResult)
      case Some(mappingExpression:MappingExpression) =>
        if(executed) {
          executionResult.parentExecutionInternal.map(parent => {
            val initialValue:Result[OpenlawExecutionState] = Success(parent)
            mappingExpression.expression.variables(parent)
              .flatMap(name => parent.getVariable(name))
              .foldLeft(initialValue)((parentExecution,subVariable) => parentExecution.flatMap(pe => executeVariable(pe, subVariable)))
          })
        }
        Success(executionResult)
      case Some(_:VariableAliasing) =>
        Failure(s"${variable.name} was previously defined as an alias. It cannot be defined as a variable")
      case None =>
        processNewVariable(executionResult, variable, executed)
    }
  }

  private def executeConditionalBlock(executionResult: OpenlawExecutionState, templates:Map[TemplateSourceIdentifier, CompiledTemplate], subBlock: Block, elseSubBlock:Option[Block], expr: Expression):Result[OpenlawExecutionState] = {
    expr.validate(executionResult).flatMap { _ =>
      val exprType = expr match {
        case variable:VariableDefinition =>
          processVariable(executionResult, variable, executed = true)
          executionResult.getVariable(variable.name) match {
            case Some(definedVariable) if definedVariable.nameOnly =>
              addNewVariable(executionResult, variable)
            case _ =>
          }
          YesNoType
        case _ =>
          executionResult.executedVariablesInternal appendAll expr.variables(executionResult)
          expr.expressionType(executionResult)
      }

      if (exprType === YesNoType) {
        if(expr.evaluate(executionResult).exists(VariableType.convert[Boolean])) {
          executionResult.remainingElements.prependAll(subBlock.elems)
          Success(executionResult)
        } else {
          elseSubBlock.map(_.elems).map(elems => {
            executionResult.remainingElements.prependAll(elems)
            executionResult
          }).getOrElse(executionResult)
          expr.validate(executionResult).map(_ => executionResult)
        }
      } else {
        Failure(s"Conditional expression $expr is of type $exprType instead of YesNo")
      }
    }
  }
}
