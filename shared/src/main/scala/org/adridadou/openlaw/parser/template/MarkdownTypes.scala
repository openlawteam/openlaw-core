package org.adridadou.openlaw.parser.template

import cats.implicits._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.TemplateParameters

/**
  * Created by davidroon on 06.06.17.
  */

trait TemplatePart

case class TemplateText(elem: Seq[TemplatePart]) extends TemplatePart

case object EmptyTemplatePart extends TemplatePart

trait ConstantExpression extends Expression {

  def typeFunction: TemplateExecutionResult => VariableType

  override def expressionType(executionResult: TemplateExecutionResult): VariableType = typeFunction(executionResult)

  override def validate(executionResult: TemplateExecutionResult): Option[String] = None

  override def variables(executionResult: TemplateExecutionResult): Seq[VariableName] = Seq()

  override def missingInput(executionResult: TemplateExecutionResult): Either[String, Seq[VariableName]] =
    Right(Seq())
}

case class NoopConstant(varType:VariableType) extends ConstantExpression {
  override def typeFunction: TemplateExecutionResult => VariableType = _ => varType
  override def evaluate(executionResult: TemplateExecutionResult): Option[Any] = None
}

case class StringConstant(value:String, typeFunction: TemplateExecutionResult => VariableType = _ => TextType) extends ConstantExpression {
  override def evaluate(executionResult: TemplateExecutionResult): Option[Any] = {
    Some(typeFunction(executionResult).cast(value, executionResult))
  }

  override def toString: String = "\"" + value + "\""
}

case class JsonConstant(value:String, typeFunction: TemplateExecutionResult => VariableType = _ => TextType) extends ConstantExpression {
  override def evaluate(executionResult: TemplateExecutionResult): Option[Any] =
    Some(typeFunction(executionResult).cast(value, executionResult))


  override def toString: String = value
}

case class NumberConstant(value:BigDecimal, typeFunction: TemplateExecutionResult => VariableType = _ => NumberType) extends ConstantExpression {
  override def evaluate(executionResult: TemplateExecutionResult): Option[Any] =
    Some(typeFunction(executionResult).cast(value.toString(), executionResult))


  override def toString: String = value.toString()
}

case class Table(header: Seq[Seq[TemplatePart]], rows: Seq[Seq[Seq[TemplatePart]]]) extends TemplatePart

trait ConditionalExpression {
  def evaluate(params:TemplateParameters):Boolean
}

case class ConditionalBlock(block:Block, conditionalExpression:Expression) extends TemplatePart
case class ForEachBlock(variable:VariableName, expression: Expression, block:Block) extends TemplatePart {
  def toCompiledTemplate(executionResult: TemplateExecutionResult):Either[String, (CompiledTemplate, VariableType)] = {
    expression.expressionType(executionResult) match {
      case listType:CollectionType =>
        val newVariable = VariableDefinition(variable, Some(VariableTypeDefinition(listType.typeParameter.name)))
        val specialCodeBlock = CodeBlock(Seq(newVariable))

        Right(CompiledDeal(
          TemplateHeader(),
          Block(Seq(specialCodeBlock) ++ block.elems),
          VariableRedefinition(),
          executionResult.clock), listType.typeParameter)
      case otherType =>
        Left(s"for each expression should be a collection but is ${otherType.getClass.getSimpleName}")
    }
  }
}
case class ConditionalBlockSet(blocks:Seq[ConditionalBlock]) extends TemplatePart

case object AEnd extends TemplatePart

case class CodeBlock(elems:Seq[TemplatePart]) extends TemplatePart {

  def smartContractCalls(): Seq[EthereumSmartContractCall] = elems.flatMap({
    case elem:EthereumSmartContractCall => Some(elem)
    case _ => None
  })
}

sealed trait ValidationResult
case object ValidationSuccess extends ValidationResult
case object ValidationError extends ValidationResult

case class Section(uuid:String, definition:Option[SectionDefinition], lvl:Int) extends TemplatePart {

  private def getSingleExpression(param: Parameter): Option[Expression] = param match {
    case OneValueParameter(expr) => Some(expr)
    case _ => None
  }

  def overrideSymbol(executionResult: TemplateExecutionResult): Option[SectionSymbol] =
    localOverrideSymbol(executionResult) match {
      case symbol @ Some(_) => symbol
      case None =>
        executionResult
          .allProcessedSections
          .map(_._1)
          .reverse
          .filter(s => s.lvl === lvl)
          .map(s => s.localOverrideSymbol(executionResult))
          .collectFirst { case Some(symbol) => symbol }
    }

  def overrideFormat(executionResult: TemplateExecutionResult): Option[SectionFormat] =
    localOverrideFormat(executionResult) match {
      case symbol @ Some(_) => symbol
      case None =>
        executionResult
          .allProcessedSections
          .map(_._1)
          .reverse
          .dropWhile(s => s === this)
          .filter(s => s.lvl === lvl)
          .map(s => s.localOverrideFormat(executionResult))
          .collectFirst { case Some(format) => format }
    }

  private def localOverrideSymbol(executionResult: TemplateExecutionResult): Option[SectionSymbol] =
    for {
      definition <- definition
      parameters <- definition.parameters
      parameter <- parameters.parameterMap.toMap.get("symbol")
      expr <- getSingleExpression(parameter)
      name <- expr.evaluate(executionResult)
      result <- SectionSymbol.withNameOption(VariableType.convert[String](name))
    } yield result

  private def localOverrideFormat(executionResult: TemplateExecutionResult): Option[SectionFormat] =
    for {
      definition <- definition
      parameters <- definition.parameters
      parameter <- parameters.parameterMap.toMap.get("format")
      expr <- getSingleExpression(parameter)
      name <- expr.evaluate(executionResult)
      result <- SectionFormat.withNameOption(VariableType.convert[String](name))
    } yield result
}

object TextElement {
  def isEmpty(elem: TextElement): Boolean = elem match {
    case Text(str) => str.isEmpty
    case _ => false
  }
}
trait TextElement extends TemplatePart
case class Text(str: String) extends TextElement
case object Em extends TextElement
case object Strong extends TextElement
case object PageBreak extends TextElement
case object Centered extends TextElement
case object RightAlign extends TextElement
case object RightThreeQuarters extends TextElement
case object ParagraphSeparator extends TextElement
case object Indent extends TextElement