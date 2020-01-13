package org.adridadou.openlaw.parser.template

import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions._


final case class Block(elems:List[TemplatePart] = Nil) {
  def variableAliases():List[VariableAliasing] = variableAliases(elems)

  def variableAliases(e:List[TemplatePart]):List[VariableAliasing] = e.flatMap({
    case variable:VariableAliasing => List(variable)
    case ConditionalBlock(block, elseBlock, _) => block.variableAliases() ++ elseBlock.map(_.variableAliases()).getOrElse(Nil)
    case ConditionalBlockSet(blocks) => variableAliases(blocks)
    case CodeBlock(e2) => variableAliases(e2)
    case _ => Nil
  })

  def variables():List[VariableDefinition] =
    variables(elems, variableAliases())

  private def variables(e:List[TemplatePart], aliases:List[VariableAliasing]):List[VariableDefinition] = e.flatMap({
    case variable:VariableDefinition if variable.name.name.nonEmpty && !aliases.exists(_.name === variable.name) => Some(variable)
    case ConditionalBlock(block, elseBlock, conditionalExpression) => block.variables() ++ elseBlock.map(_.variables()).getOrElse(Nil) ++ expressionVariables(conditionalExpression)
    case ConditionalBlockSet(blocks) => variables(blocks, aliases)
    case ForEachBlock(_, expression, block) => block.variables() ++ expressionVariables(expression)
    case CodeBlock(e2) => variables(e2, aliases)
    case section:VariableSection => section.variables
    case TemplateText(subElems) => variables(subElems, aliases)
    case _ => None
  }).filter(_.name.name.trim.nonEmpty)

  private def expressionVariables(expression: Expression):List[VariableDefinition] = {
    expression match {
      case variable:VariableDefinition => List(variable)
      case ComparisonExpression(expr1, expr2, _) => expressionVariables(expr1) ++ expressionVariables(expr2)
      case EqualsExpression(expr1, expr2) => expressionVariables(expr1) ++ expressionVariables(expr2)
      case BooleanExpression(expr1, expr2, _) => expressionVariables(expr1) ++ expressionVariables(expr2)
      case BooleanUnaryExpression(expr, _) => expressionVariables(expr)
      case _ => Nil
    }
  }
}
