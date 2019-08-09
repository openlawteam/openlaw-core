package org.adridadou.openlaw.parser.template

import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.values.TemplateParameters
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.adridadou.openlaw.{OpenlawString, OpenlawValue}

/**
  * Created by davidroon on 06.06.17.
  */

trait TemplatePart

case class TemplateText(elem: Seq[TemplatePart]) extends TemplatePart

case object EmptyTemplatePart extends TemplatePart

trait ConstantExpression extends Expression {

  def typeFunction: TemplateExecutionResult => VariableType

  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] = Success(typeFunction(executionResult))

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] = Success(())

  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] = Success(Seq())

  override def missingInput(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] = Success(Seq())
}

case class NoopConstant(varType:VariableType) extends ConstantExpression {
  override def typeFunction: TemplateExecutionResult => VariableType = _ => varType
  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = Success(None)
}

case class StringConstant(value:String, typeFunction: TemplateExecutionResult => VariableType = _ => TextType) extends ConstantExpression {
  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    typeFunction(executionResult).cast(value, executionResult).map(Some(_))

  override def toString: String = "\"" + value + "\""
}

case class JsonConstant(value:String, typeFunction: TemplateExecutionResult => VariableType = _ => TextType) extends ConstantExpression {
  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    typeFunction(executionResult).cast(value, executionResult).map(Some(_))


  override def toString: String = value
}

case class NumberConstant(value:BigDecimal, typeFunction: TemplateExecutionResult => VariableType = _ => NumberType) extends ConstantExpression {
  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    typeFunction(executionResult).cast(value.toString(), executionResult).map(Some(_))


  override def toString: String = value.toString()
}

case class Table(header: List[List[TemplatePart]], rows: List[List[List[TemplatePart]]]) extends TemplatePart

trait ConditionalExpression {
  def evaluate(params:TemplateParameters):Boolean
}

case class ConditionalBlock(block:Block, elseBlock:Option[Block], conditionalExpression:Expression) extends TemplatePart
case class ForEachBlock(variable:VariableName, expression: Expression, block:Block) extends TemplatePart {
  def toCompiledTemplate(executionResult: TemplateExecutionResult): Result[(CompiledTemplate, VariableType)] = {
    expression.expressionType(executionResult).flatMap {
      case listType:CollectionType =>
        val newVariable = VariableDefinition(variable, Some(VariableTypeDefinition(listType.typeParameter.name)))
        val specialCodeBlock = CodeBlock(Seq(newVariable))

        Success(CompiledDeal(
          TemplateHeader(),
          Block(Seq(specialCodeBlock) ++ block.elems),
          VariableRedefinition(),
          executionResult.clock), listType.typeParameter)
      case otherType =>
        Failure(s"for each expression should be a collection but is ${otherType.getClass.getSimpleName}")
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

object Section {
  implicit val sectionEnc:Encoder[Section] = deriveEncoder[Section]
  implicit val sectionDec:Decoder[Section] = deriveDecoder[Section]
}

case class Section(uuid:String, definition:Option[SectionDefinition], lvl:Int) extends TemplatePart {

  private def getSingleExpression(param: Parameter): Option[Expression] = param match {
    case OneValueParameter(expr) => Some(expr)
    case _ => None
  }

  def overrideSymbol(executionResult: TemplateExecutionResult): Result[Option[SectionSymbol]] =
    localOverrideSymbol(executionResult).flatMap {
      case symbol @ Some(_) => Success(symbol)
      case None =>
        executionResult
          .allProcessedSections
          .map({case (section,_) => section})
          .reverse
          .filter(s => s.lvl === lvl)
          .map(s => s.localOverrideSymbol(executionResult))
          .toList
          .sequence
          .map(_.collectFirst { case Some(symbol) => symbol })
    }

  def overrideFormat(executionResult: TemplateExecutionResult): Result[Option[SectionFormat]] =
    localOverrideFormat(executionResult).flatMap {
      case format @ Some(_) => Success(format)
      case None =>
        executionResult
          .allProcessedSections
          .map({case (section,_) => section})
          .reverse
          .dropWhile(s => s === this)
          .filter(s => s.lvl === lvl)
          .map(s => s.localOverrideFormat(executionResult))
          .toList
          .sequence
          .map(_.collectFirst { case Some(format) => format })
    }

  private def localOverrideSymbol(executionResult: TemplateExecutionResult): Result[Option[SectionSymbol]] =
    (for {
      definition <- definition
      parameters <- definition.parameters
      parameter <- parameters.parameterMap.toMap.get("symbol")
      expr <- getSingleExpression(parameter)
    } yield {
      expr
        .evaluate(executionResult)
        .flatMap(_.map(VariableType.convert[OpenlawString]).sequence)
        .map(_.flatMap(SectionSymbol.withNameOption))
    })
    .sequence
    .map(_.flatten)

  private def localOverrideFormat(executionResult: TemplateExecutionResult): Result[Option[SectionFormat]] =
    (for {
      definition <- definition
      parameters <- definition.parameters
      parameter <- parameters.parameterMap.toMap.get("format")
      expr <- getSingleExpression(parameter)
    } yield {
      expr
        .evaluate(executionResult)
        .flatMap(_.map(VariableType.convert[OpenlawString]).sequence)
        .map(_.flatMap(SectionFormat.withNameOption))
    })
    .sequence
    .map(_.flatten)
}

object TextElement {

  def isEmpty(elem: TextElement): Boolean = elem match {
    case Text(str) => str.isEmpty
    case _ => false
  }

  implicit val textElementEnc:Encoder[TextElement] = (a: TextElement) => {
    val nameField = "name" -> Json.fromString(a.elementTypeName)

    a match {
      case t:Text => Json.obj(
        nameField,
        "value" -> t.asJson
      )
      case v:VariableDefinition => Json.obj(
        nameField,
        "value" -> v.asJson
      )
      case _ => Json.obj(
        nameField
      )
    }
  }

  implicit val textElementDec:Decoder[TextElement] = (c: HCursor) => {
    c.downField("name").as[String]
      .flatMap(decodeElement(_, c))
  }

  private def decodeElement(name:String, c:HCursor):Decoder.Result[TextElement] = {
    name match {
      case _ if "Text" === name =>
        c.downField("value").as[Text]
      case _ if "VariableDefinition" === name =>
        c.downField("value").as[VariableDefinition]
      case _ if "Em" === name =>
        Right(Em)
      case _ if "Strong" === name =>
        Right(Strong)
      case _ if "Under" === name =>
        Right(Under)
      case _ if "PageBreak" === name =>
        Right(PageBreak)
      case _ if "SectionBreak" === name =>
        Right(SectionBreak)
      case _ if "Centered" === name =>
        Right(Centered)
      case _ if "RightAlign" === name =>
        Right(RightAlign)
      case _ if "RightThreeQuarters" === name =>
        Right(RightThreeQuarters)
      case _ if "ParagraphSeparator" === name =>
        Right(ParagraphSeparator)
      case _ if "Indent" === name =>
        Right(Indent)
      case _ =>
        Left(DecodingFailure(s"unknown text element type $name", List()))
    }
  }
}

abstract class TextElement(val elementTypeName:String) extends TemplatePart
case class Text(str: String) extends TextElement("Text")
case object Em extends TextElement("Em")
case object Strong extends TextElement("Strong")
case object Under extends TextElement("Under")
case object PageBreak extends TextElement("PageBreak")
case object SectionBreak extends TextElement("SectionBreak")
case object Centered extends TextElement("Centered")
case object RightAlign extends TextElement("RightAlign")
case object RightThreeQuarters extends TextElement("RightThreeQuarters")
case object ParagraphSeparator extends TextElement("ParagraphSeparator")
case object Indent extends TextElement("Indent")