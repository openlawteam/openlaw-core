package org.adridadou.openlaw.vm

import java.util.concurrent.atomic.AtomicInteger

import org.adridadou.openlaw.result._
import org.adridadou.openlaw.result.Implicits.RichEither
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.TemplateParameters
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.printers.SectionHelper

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

class OpenlawExecutionEngine extends VariableExecutionEngine {

  /**
    * Entry point. This is where you start the execution of the main template
    */
  def execute(mainTemplate:CompiledTemplate, parameters:TemplateParameters, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[TemplateExecutionResult] =
    execute(mainTemplate, parameters, templates, Map())

  def execute(mainTemplate:CompiledTemplate, parameters:TemplateParameters, templates:Map[TemplateSourceIdentifier, CompiledTemplate], signatureProofs:Map[Email, SignatureProof]):Result[TemplateExecutionResult] = {
    val executionResult = TemplateExecutionResult(
      parameters = parameters,
      id = TemplateExecutionResultId(s"@@anonymous_main_template_id@@"),
      template = mainTemplate,
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
              newResult.agreements.append(t)
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
  final def resumeExecution(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[TemplateExecutionResult] = {
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

      case ExecutionWaitForTemplate(variableName, identifier) =>
        templates.get(identifier) match {
          case Some(template) =>
            // has to be in a matcher for tail call optimization
            attempt(executionResult.startTemplateExecution(variableName, template)).flatMap(_.toResult) match {
              case Success(result) => resumeExecution(result, templates)
              case f => f
            }
          case None => Success(executionResult)
        }

      case ExecutionReady =>
        // has to be in a matcher for tail call optimization
        executeInternal(executionResult, templates) match {
          case Success(result) => resumeExecution(result, templates)
          case f => f
        }
    }
  }

  private def finishExecution(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[TemplateExecutionResult] = {
    executionResult.parentExecution.map(parent => {
      (for {
        definition <- executionResult.templateDefinition
      } yield {
        executionResult.template match {
          case agreement:CompiledAgreement =>
            Try(getRoot(parent).agreements.append(executionResult.structuredInternal(agreement)))
              .toEither
              .left
              .map(_.getMessage )

          case _ =>
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

  private final def executeInternal(execution: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate]): Result[TemplateExecutionResult] = {
    execution.embeddedExecutions.headOption match {
      case Some(embeddedExecution) =>
        execution.embeddedExecutions.remove(0)
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

  private def processVariableMember(executionResult: TemplateExecutionResult, variableMember: VariableMember, executed:Boolean):Result[TemplateExecutionResult] = {
    processVariable(executionResult, VariableDefinition(name = variableMember.name), executed)
    variableMember.validate(executionResult).map(_ => executionResult)
  }

  private def processExecutedElement(executionResult: TemplateExecutionResult, element: TemplatePart, templates:Map[TemplateSourceIdentifier, CompiledTemplate]): Result[TemplateExecutionResult] = element match {
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
      val initialValue:Result[TemplateExecutionResult] = Success(executionResult)
      (header.flatten ++ rows.flatten.flatten)
        .foldLeft(initialValue)((exec, elem) => exec.flatMap(processExecutedElement(_, elem, templates)))

    case ConditionalBlockSet(blocks) =>
      executeConditionalBlockSet(executionResult, blocks)

    case CodeBlock(elems) =>
      val initialValue:Result[TemplateExecutionResult] = Success(executionResult)
      elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))

    case section:VariableSection =>
      val initialValue:Result[TemplateExecutionResult] = Success(executionResult)
      section.variables
        .foldLeft(initialValue)((exec, variableDefinition) => exec.flatMap(processVariable(_, variableDefinition, executed = true)))
        .flatMap(processVariableSection(_, section))

    case section:Section =>
      executionResult.sectionLevelStack append section.lvl
      processSection(section, executionResult)

    case _ =>
      Success(executionResult)
  }

  private def generateFullSectionValue(section:Section, sectionValue:String, executionResult: TemplateExecutionResult):String = {
    val fullSectionValue = (1 to section.lvl).map({
      case section.lvl => sectionValue
      case idx => executionResult.getLastSectionByLevel(idx)
    }).mkString(".")
    fullSectionValue
  }

  private def processSection(section: Section, executionResult: TemplateExecutionResult): Result[TemplateExecutionResult] = {
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
      executionResult.variables.append(variable)
      executionResult.executedVariables.append(name)
      executionResult.sectionNameMapping put (section.uuid , name)
      executionResult.addLastSectionByLevel(section.lvl, referenceValue)
      executionResult.addSectionLevelStack(newSectionValues)
      executionResult.addProcessedSection(section, SectionHelper.calculateNumberInList(section.lvl, numbering))

      executionResult
    })
  }


  private def findOverriddenFormat(executionResult: TemplateExecutionResult, section: Section, previous: Seq[Section]): Option[SectionFormat] =
    section.overrideFormat(executionResult).orElse {
      previous
        .reverse
        .filter(s => s.lvl === section.lvl)
        .map(s => s.overrideFormat(executionResult))
        .collectFirst { case Some(format) => format }
    }

  private def executeForEachBlock(executionResult: TemplateExecutionResult, foreachBlock: ForEachBlock):Result[TemplateExecutionResult] = {
    foreachBlock.toCompiledTemplate(executionResult).toResult.flatMap({ case (template, expressionType) =>
      val initialValue: Result[TemplateExecutionResult] = Success(executionResult)
      executionResult.executedVariables appendAll foreachBlock.expression.variables(executionResult)

      val elements = foreachBlock.expression.evaluate(executionResult)
        .map(value => VariableType.convert[CollectionValue](value).list)
        .getOrElse(Seq())

      elements.foldLeft(initialValue)((eitherExecutionResult, element) => eitherExecutionResult.flatMap(_ => {
        val anonymousVariable = executionResult.createAnonymousVariable()
        executionResult.variables append VariableDefinition(name = anonymousVariable, variableTypeDefinition = Some(VariableTypeDefinition(TemplateType.name)), defaultValue = Some(OneValueParameter(StringConstant(anonymousVariable.name))))
        executionResult.executedVariables append anonymousVariable

        executionResult.startEmbeddedExecution(anonymousVariable, template, foreachBlock.variable, element, expressionType).map(_ => executionResult).toResult
      })
      )
    })
  }

  private def executeConditionalBlockSet(executionResult: TemplateExecutionResult, blocks: Seq[ConditionalBlock]) = {
    blocks.foreach({
      subBlock =>
        subBlock.conditionalExpression match {
          case variable: VariableDefinition =>
            processVariable(executionResult, variable, executed = true)
          case _ =>
            executionResult.executedVariables appendAll subBlock.conditionalExpression.variables(executionResult)
        }
    })

    blocks.find(_.conditionalExpression.evaluate(executionResult).exists(VariableType.convert[Boolean]))
      .map(subBlock => {
        executionResult.remainingElements.prependAll(subBlock.block.elems)
        Success(executionResult)
      }).getOrElse(Success(executionResult))
  }

  private def getRoot(parent:TemplateExecutionResult):TemplateExecutionResult = parent.parentExecution match {
    case Some(parentExecution) => getRoot(parentExecution)
    case None => parent
  }

  private def validateSubExecution(result: TemplateExecutionResult, templateDefinition: TemplateDefinition): Result[TemplateExecutionResult] = {
    val initialValue:Result[TemplateExecutionResult] = Success(result)
    result.getVariables
      .foldLeft(initialValue)((value, variable) =>
        value.flatMap(_ => validateSubExecution(result, templateDefinition, variable))
      )
  }

  private def validateSubExecution(result:TemplateExecutionResult, currentTemplateDefinition:TemplateDefinition, variable:VariableDefinition): Result[TemplateExecutionResult] = {
    val otherType = result.getVariable(variable.name).map(_.varType(result)).getOrElse(variable.varType(result))
    if(otherType =!= variable.varType(result)) {
      val typeString = variable.variableTypeDefinition.map(_.name).getOrElse("<undefined>")
      Failure(s"Variable definition mismatch. variable ${variable.name} is defined as $typeString in ${currentTemplateDefinition.name.name} but was ${otherType.name} in ${result.templateDefinition.map(_.name.name.title).getOrElse("the main template")}")
    } else {
      result.parentExecution.map(parent => validateSubExecution(parent, currentTemplateDefinition, variable)).getOrElse(Success(result))
    }
  }

  private def processVariableSection(executionResult: TemplateExecutionResult, section: VariableSection):Result[TemplateExecutionResult] = {
    val existingSection = executionResult.variableSectionsInternal.getOrElse(section.name, collection.mutable.Buffer[VariableName]())
    existingSection.appendAll(section.variables.map(_.name))

    if(!executionResult.variableSectionsInternal.contains(section.name)) {
      executionResult.variableSectionList.append(section.name)
    }

    executionResult.variableSectionsInternal.put(section.name, existingSection)
    Success(executionResult)
  }

  private def processCodeElement(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate], element:TemplatePart): Result[TemplateExecutionResult] = element match {
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
      val initialValue:Result[TemplateExecutionResult] = Success(executionResult)
      section.variables.foldLeft(initialValue)((exec, variableDefinition) => exec.flatMap(e => processVariable(e, variableDefinition, executed = false)))
      processVariableSection(executionResult, section)

    case _ =>
      Success(executionResult)
  }

  private def processConditionalBlockSet(executionResult: TemplateExecutionResult, blocks: Seq[ConditionalBlock]) = {
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

  private def processConditionalBlock(executionResult: TemplateExecutionResult, block: Block, elseBlock:Option[Block], expression: Expression, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Result[TemplateExecutionResult] = {
    expression match {
      case variable:VariableDefinition =>
        processVariable(executionResult, variable, executed = false)
      case _ =>
    }
    if(expression.evaluate(executionResult).exists(VariableType.convert[Boolean])) {
      val initialValue:Result[TemplateExecutionResult] = Success(executionResult)
      val initialValue2 = block.elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))
      elseBlock.map(_.elems.foldLeft(initialValue2)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))).getOrElse(initialValue2)
    } else {
      Success(executionResult)
    }
  }

  private def processVariable(executionResult: TemplateExecutionResult, variable: VariableDefinition, executed:Boolean): Result[TemplateExecutionResult] = {
    executionResult.getExpression(variable.name) match {
      case Some(_:VariableDefinition) =>
        processDefinedVariable(executionResult, variable, executed).toResult
      case Some(alias:VariableAliasing) if variable.nameOnly =>
        executionResult.executedVariables appendAll alias.expr.variables(executionResult)
        Success(executionResult)
      case Some(mappingExpression:MappingExpression) =>
        if(executed) {
          executionResult.parentExecution.map(parent => {
            val initialValue:Result[TemplateExecutionResult] = Success(parent)
            mappingExpression.expression.variables(parent)
              .flatMap(name => parent.getVariable(name))
              .foldLeft(initialValue)((parentExecution,subVariable) => parentExecution.flatMap(pe => executeVariable(pe, subVariable).toResult))
          })
        }
        Success(executionResult)
      case Some(_:VariableAliasing) =>
        Failure(s"${variable.name} was previously defined as an alias. It cannot be defined as a variable")
      case None =>
        processNewVariable(executionResult, variable, executed)
    }
  }

  private def executeConditionalBlock(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate], subBlock: Block, elseSubBlock:Option[Block], expr: Expression):Result[TemplateExecutionResult] = {
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
          executionResult.executedVariables appendAll expr.variables(executionResult)
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

  private def executeConditionalBlockWithElse(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate], subBlock: Block, elseSubBlock: Option[Block], expr: Expression): Result[TemplateExecutionResult] = {
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
          executionResult.executedVariables appendAll expr.variables(executionResult)
          expr.expressionType(executionResult)
      }

      if(exprType === YesNoType) {
        if(expr.evaluate(executionResult).exists(VariableType.convert[Boolean])) {
          executionResult.remainingElements.prependAll(subBlock.elems)
          Success(executionResult)
        } else {
          elseSubBlock.map(_.elems).map(elems => {
            executionResult.remainingElements.prependAll(elems)
            Success(executionResult)
          }).getOrElse(Success(executionResult))
        }
      } else {
        Failure(s"Conditional expression $expr is of type $exprType instead of YesNo")
      }
    }
  }
}
