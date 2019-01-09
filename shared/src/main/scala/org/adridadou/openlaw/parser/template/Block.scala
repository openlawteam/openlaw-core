package org.adridadou.openlaw.parser.template

import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions._


case class Block(elems:Seq[TemplatePart] = Seq()) {
  def variableAliases():Seq[VariableAliasing] = variableAliases(elems)

  def variableAliases(e:Seq[TemplatePart]):Seq[VariableAliasing] = e.flatMap({
    case variable:VariableAliasing => Some(variable)
    case ConditionalBlock(block, _) => block.variableAliases()
    case ConditionalBlockSet(blocks) => variableAliases(blocks)
    case ConditionalBlockWithElse(ifBlock, elseBlock, _) =>
      ifBlock.variableAliases()
      elseBlock.variableAliases()
    case ConditionalBlockSetWithElse(blocks) => variableAliases(blocks)
    case CodeBlock(e2) => variableAliases(e2)
    case _ => None
  })

  def variables():Seq[VariableDefinition] =
    variables(elems, variableAliases())

  private def variables(e:Seq[TemplatePart], aliases:Seq[VariableAliasing]):Seq[VariableDefinition] = e.flatMap({
    case variable:VariableDefinition if variable.name.name.nonEmpty && !aliases.exists(_.name === variable.name) => Some(variable)
    case ConditionalBlock(block, conditionalExpression) => block.variables() ++ expressionVariables(conditionalExpression)
    case ConditionalBlockSet(blocks) => variables(blocks, aliases)
    case ConditionalBlockWithElse(ifBlock, elseBlock, conditionalExpression) =>
      ifBlock.variables() ++ expressionVariables(conditionalExpression)
      elseBlock.variables() ++ expressionVariables(conditionalExpression)
    case ConditionalBlockSetWithElse(blocks) =>
      variables(blocks, aliases)
    case ForEachBlock(_, expression, block) => block.variables() ++ expressionVariables(expression)
    case CodeBlock(e2) => variables(e2, aliases)
    case section:VariableSection => section.variables
    case TemplateText(subElems) => variables(subElems, aliases)
    case _ => None
  }).filter(_.name.name.trim.nonEmpty)

  private def expressionVariables(expression: Expression):Seq[VariableDefinition] = {
    expression match {
      case variable:VariableDefinition => Seq(variable)
      case ComparaisonExpression(expr1, expr2, _) => expressionVariables(expr1) ++ expressionVariables(expr2)
      case EqualsExpression(expr1, expr2) => expressionVariables(expr1) ++ expressionVariables(expr2)
      case BooleanExpression(expr1, expr2, _) => expressionVariables(expr1) ++ expressionVariables(expr2)
      case BooleanUnaryExpression(expr, _) => expressionVariables(expr)
      case _ => Seq()
    }
  }
}
