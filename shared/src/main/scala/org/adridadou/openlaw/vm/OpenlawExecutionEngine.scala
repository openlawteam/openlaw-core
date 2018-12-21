package org.adridadou.openlaw.vm

import java.util.concurrent.atomic.AtomicInteger

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.TemplateParameters
import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.printers.SectionHelper

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class OpenlawExecutionEngine extends VariableExecutionEngine {

  /**
    * Entry point. This is where you start the execution of the main template
    */
  def execute(mainTemplate:CompiledTemplate, parameters:TemplateParameters, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Either[String, TemplateExecutionResult] =
    execute(mainTemplate, parameters, templates, Map())

  def execute(mainTemplate:CompiledTemplate, parameters:TemplateParameters, templates:Map[TemplateSourceIdentifier, CompiledTemplate], signatureProofs:Map[Email, SignatureProof]):Either[String, TemplateExecutionResult] = {
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
          Try(executionResult.structuredMainTemplate(agreement))
            .map { t =>
              newResult.agreements.append(t)
              newResult
            }
            .toEither
            .left
            .map(_.getMessage)
        case _ =>
          Right(newResult)
      }
    })
  }

  /**
    * This method is used if the execution stops due to a missing template and you want to resume the execution
    */
  @tailrec
  final def resumeExecution(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Either[String, TemplateExecutionResult] = {
    executionResult.state match {
      case ExecutionFinished =>
        executionResult.parentExecution match {
          case Some(_) =>
            Try(finishExecution(executionResult, templates)) match {
              case Success(Left(ex)) =>
                Left(ex)
              case Success(Right(result)) =>
                resumeExecution(result, templates)
              case Failure(ex) =>
                Left(ex.getMessage)
            }
          case None =>
            Right(executionResult)
        }
      case ExecutionWaitForTemplate(variableName, identifier) =>
        templates.get(identifier) match {
          case Some(template) =>
            Try(executionResult.startTemplateExecution(variableName, template)) match {
              case Success(Left(ex)) =>
                Left(ex)
              case Success(Right(result)) =>
                resumeExecution(result, templates)
              case Failure(ex) =>
                Left(ex.getMessage)
            }
          case None =>
            Right(executionResult)
        }

      case ExecutionReady =>
        executeInternal(executionResult, templates) match {
          case Left(ex) =>
            Left(ex)
          case Right(result) =>
            resumeExecution(result, templates)
        }
    }
  }

  private def finishExecution(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Either[String, TemplateExecutionResult] = {
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

      }).getOrElse(Right(parent)) map (_.copy(state = ExecutionReady))

    }).getOrElse(Right(executionResult))
  }

  private final def executeInternal(execution: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate]): Either[String, TemplateExecutionResult] = {
    execution.embeddedExecutions.headOption match {
      case Some(embeddedExecution) =>
        execution.embeddedExecutions.remove(0)
        Right(embeddedExecution)
      case _ =>
        execution.remainingElements.headOption match {
          case Some(elem) =>
            execution.remainingElements.remove(0)
            processExecutedElement(execution, elem, templates)
          case None =>
            Right(execution.copy(state = ExecutionFinished))
        }
    }
  }

  private def processVariableMember(executionResult: TemplateExecutionResult, variableMember: VariableMember, executed:Boolean):Either[String,TemplateExecutionResult] = {
    processVariable(executionResult, VariableDefinition(name = variableMember.name), executed)
    variableMember.validate(executionResult) match {
      case Some(err) => Left(err)
      case None => Right(executionResult)
    }
  }

  private def processExecutedElement(executionResult: TemplateExecutionResult, element: TemplatePart, templates:Map[TemplateSourceIdentifier, CompiledTemplate]): Either[String, TemplateExecutionResult] = element match {
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
      Right(executionResult)
    case ConditionalBlock(subBlock, expr) =>
      executeConditionalBlock(executionResult, templates, subBlock, expr)
    case ConditionalBlockWithElse(subBlock, subBlock2, expr) =>
      executeConditionalBlockWithElse(executionResult, templates, subBlock, subBlock2, expr)
    case foreachBlock:ForEachBlock =>
      executeForEachBlock(executionResult, foreachBlock)

    case Table(header, rows) =>
      val initialValue:Either[String, TemplateExecutionResult] = Right(executionResult)
      (header.flatten ++ rows.flatten.flatten)
        .foldLeft(initialValue)((exec, elem) => exec.flatMap(processExecutedElement(_, elem, templates)))

    case ConditionalBlockSet(blocks) =>
      executeConditionalBlockSet(executionResult, blocks)

    case ConditionalBlockSetWithElse(blocks) =>
      executeConditionalBlockSetWithElse(executionResult, blocks)

    case CodeBlock(elems) =>
      val initialValue:Either[String, TemplateExecutionResult] = Right(executionResult)
      elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))

    case section:VariableSection =>
      val initialValue:Either[String, TemplateExecutionResult] = Right(executionResult)
      section.variables
        .foldLeft(initialValue)((exec, variableDefinition) => exec.flatMap(processVariable(_, variableDefinition, executed = true)))
          .flatMap(processVariableSection(_, section))

    case section:Section =>
      executionResult.sectionLevelStack append section.lvl
      processSection(section, executionResult)

    case _ =>
      Right(executionResult)
  }

  private def generateFullSectionValue(section:Section, sectionValue:String, executionResult: TemplateExecutionResult):String = {
    val fullSectionValue = (1 to section.lvl).map({
      case section.lvl => sectionValue
      case idx => executionResult.getLastSectionByLevel(idx)
    }).mkString(".")
    fullSectionValue
  }

  private def processSection(section: Section, executionResult: TemplateExecutionResult): Either[String, TemplateExecutionResult] = {
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

        Right(values,  -1 :: allValues)
      case Some(badValue) =>
        Left(s"numbering parameter in section definition should be a number, not ${badValue.getClass.getSimpleName}")
      case None =>
        Right(executionResult.sectionLevelStack, Nil)
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
    section.overrideFormat(executionResult) match {
      case format @ Some(_) => format
      case None =>
        previous
          .reverse
          .filter(s => s.lvl === section.lvl)
          .map(s => s.overrideFormat(executionResult))
          .collectFirst { case Some(format) => format }
    }

  private def executeForEachBlock(executionResult: TemplateExecutionResult, foreachBlock: ForEachBlock):Either[String, TemplateExecutionResult] = {
    foreachBlock.toCompiledTemplate(executionResult).flatMap({ case (template, expressionType) =>
      val initialValue: Either[String, TemplateExecutionResult] = Right(executionResult)
      executionResult.executedVariables appendAll foreachBlock.expression.variables(executionResult)

      val elements = foreachBlock.expression.evaluate(executionResult)
        .map(value => VariableType.convert[CollectionValue](value).list)
        .getOrElse(Seq())

      elements.foldLeft(initialValue)((eitherExecutionResult, element) => eitherExecutionResult.flatMap(_ => {
        val anonymousVariable = executionResult.createAnonymousVariable()
        executionResult.variables append VariableDefinition(name = anonymousVariable, variableTypeDefinition = Some(VariableTypeDefinition(TemplateType.name)), defaultValue = Some(OneValueParameter(StringConstant(anonymousVariable.name))))
        executionResult.executedVariables append anonymousVariable

        executionResult.startEmbeddedExecution(anonymousVariable, template, foreachBlock.variable, element, expressionType).map(_ => executionResult)
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
        Right(executionResult)
      }).getOrElse(Right(executionResult))
  }

  private def executeConditionalBlockSetWithElse(executionResult: TemplateExecutionResult, blocks: Seq[ConditionalBlockWithElse]) = {
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
        Right(executionResult)
      }).getOrElse(Right(executionResult))
  }

  private def getRoot(parent:TemplateExecutionResult):TemplateExecutionResult = parent.parentExecution match {
    case Some(parentExecution) => getRoot(parentExecution)
    case None => parent
  }

  private def validateSubExecution(result: TemplateExecutionResult, templateDefinition: TemplateDefinition):Either[String, TemplateExecutionResult] = {
    val initialValue:Either[String, TemplateExecutionResult] = Right(result)
    result.getVariables
      .foldLeft(initialValue)((value, variable) =>
        value.flatMap(_ => validateSubExecution(result, templateDefinition, variable))
      )
  }

  private def validateSubExecution(result:TemplateExecutionResult, currentTemplateDefinition:TemplateDefinition, variable:VariableDefinition):Either[String, TemplateExecutionResult] = {
    val otherType = result.getVariable(variable.name).map(_.varType(result)).getOrElse(variable.varType(result))
    if(otherType =!= variable.varType(result)) {
      val typeString = variable.variableTypeDefinition.map(_.name).getOrElse("<undefined>")
      Left(s"Variable definition mismatch. variable ${variable.name} is defined as $typeString in ${currentTemplateDefinition.name.name} but was ${otherType.name} in ${result.templateDefinition.map(_.name.name.title).getOrElse("the main template")}")
    } else {
      result.parentExecution match {
        case Some(parent) =>
          validateSubExecution(parent, currentTemplateDefinition, variable)
        case None =>
          Right(result)
      }
    }
  }

  private def processVariableSection(executionResult: TemplateExecutionResult, section: VariableSection):Either[String, TemplateExecutionResult] = {
    val existingSection = executionResult.variableSectionsInternal.getOrElse(section.name, collection.mutable.Buffer[VariableName]())
    existingSection.appendAll(section.variables.map(_.name))

    if(!executionResult.variableSectionsInternal.contains(section.name)) {
      executionResult.variableSectionList.append(section.name)
    }

    executionResult.variableSectionsInternal.put(section.name, existingSection)
    Right(executionResult)
  }

  private def processCodeElement(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate], element:TemplatePart): Either[String, TemplateExecutionResult] = element match {
    case variable: VariableDefinition =>
      processVariable(executionResult, variable, executed = false)

    case variable:VariableName =>
      processVariable(executionResult, VariableDefinition(name = variable.name), executed = false)

    case alias:VariableAliasing =>
      processAlias(executionResult, alias, executed = false)

    case ConditionalBlock(subBlock, expr) =>
      processConditionalBlock(executionResult, subBlock, expr, templates)

    case ConditionalBlockSet(blocks) =>
      processConditionalBlockSet(executionResult, blocks)

    case ConditionalBlockWithElse(subBlock, subBlock2, expr) =>
      processConditionalBlockWithElse(executionResult, subBlock, subBlock2, expr, templates)

    case ConditionalBlockSetWithElse(blocks) =>
      processConditionalBlockSetWithElse(executionResult, blocks)

    case section:VariableSection =>
      val initialValue:Either[String, TemplateExecutionResult] = Right(executionResult)
      section.variables.foldLeft(initialValue)((exec, variableDefinition) => exec.flatMap(e => processVariable(e, variableDefinition, executed = false)))
      processVariableSection(executionResult, section)

    case _ =>
      Right(executionResult)
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

    Right(executionResult)
  }

  private def processConditionalBlock(executionResult: TemplateExecutionResult, block: Block, expression: Expression, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Either[String, TemplateExecutionResult] = {
    expression match {
      case variable:VariableDefinition =>
        processVariable(executionResult, variable, executed = false)
      case _ =>
    }
    if(expression.evaluate(executionResult).exists(VariableType.convert[Boolean])) {
      val initialValue:Either[String, TemplateExecutionResult] = Right(executionResult)
      block.elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))
    } else {
      Right(executionResult)
    }
  }

  private def processConditionalBlockSetWithElse(executionResult: TemplateExecutionResult, blocks: Seq[ConditionalBlockWithElse]) = {
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

    Right(executionResult)
  }

  private def processConditionalBlockWithElse(executionResult: TemplateExecutionResult, block: Block, block2: Block, expression: Expression, templates:Map[TemplateSourceIdentifier, CompiledTemplate]):Either[String, TemplateExecutionResult] = {
    expression match {
      case variable:VariableDefinition =>
        processVariable(executionResult, variable, executed = false)
      case _ =>
    }
    if(expression.evaluate(executionResult).exists(VariableType.convert[Boolean])) {
      val initialValue:Either[String, TemplateExecutionResult] = Right(executionResult)
      println("processing if " + block.elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem))))
      block.elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))
    } else {
      val initialValue:Either[String, TemplateExecutionResult] = Right(executionResult)
      println("processing else " + block2.elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem))))
      block2.elems.foldLeft(initialValue)((exec, elem) => exec.flatMap(processCodeElement(_, templates, elem)))
      //Right(executionResult)
    }
  }


  private def processVariable(executionResult: TemplateExecutionResult, variable: VariableDefinition, executed:Boolean): Either[String, TemplateExecutionResult] = {
    executionResult.getExpression(variable.name) match {
      case Some(_:VariableDefinition) =>
        processDefinedVariable(executionResult, variable, executed)
      case Some(alias:VariableAliasing) if variable.nameOnly =>
        executionResult.executedVariables appendAll alias.expr.variables(executionResult)
        Right(executionResult)
      case Some(mappingExpression:MappingExpression) =>
        if(executed) {
          executionResult.parentExecution.map(parent => {
            val initialValue:Either[String, TemplateExecutionResult] = Right(parent)
            mappingExpression.expression.variables(parent)
              .flatMap(name => parent.getVariable(name))
              .foldLeft(initialValue)((parentExecution,subVariable) => parentExecution.flatMap(pe => executeVariable(pe, subVariable)))
          })
        }
        Right(executionResult)
      case Some(_:VariableAliasing) =>
        Left(s"${variable.name} was previously defined as an alias. It cannot be defined as a variable")
      case None =>
        processNewVariable(executionResult, variable, executed)
    }
  }

  private def executeConditionalBlock(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate], subBlock: Block, expr: Expression):Either[String, TemplateExecutionResult] = {
    expr.validate(executionResult) match {
      case Some(err) =>
        Left(err)
      case None =>
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
            Right(executionResult)
          } else {
            expr.validate(executionResult) map {err => Left(err)} getOrElse Right(executionResult)
          }
        }else {
          Left(s"Conditional expression $expr is of type $exprType instead of YesNo")
        }
    }
  }

  private def executeConditionalBlockWithElse(executionResult: TemplateExecutionResult, templates:Map[TemplateSourceIdentifier, CompiledTemplate], subBlock: Block, subBlock2: Block, expr: Expression):Either[String, TemplateExecutionResult] = {
    expr.validate(executionResult) match {
      case Some(err) =>
        Left(err)
      case None =>
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
            println("execution result if block " + Right(executionResult))
            Right(executionResult)
          } else {
            executionResult.remainingElements.prependAll(subBlock2.elems)
            println("execution result else block " + Right(executionResult))
            Right(executionResult)
            //expr.validate(executionResult) map {err => Left(err)} getOrElse Right(executionResult)
          }
        }else {
          Left(s"Conditional expression $expr is of type $exprType instead of YesNo")
        }
    }
  }
}
