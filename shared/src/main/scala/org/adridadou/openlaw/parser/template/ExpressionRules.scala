package org.adridadou.openlaw.parser.template

import cats.kernel.Eq
import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax._
import org.adridadou.openlaw.OpenlawNativeValue
import org.adridadou.openlaw.parser.template.variableTypes._
import org.parboiled2.Rule1
import org.adridadou.openlaw.parser.template.expressions._
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.result.Implicits.RichResult

import scala.reflect.ClassTag

/**
  * Created by davidroon on 06.06.17.
  */
trait ExpressionRules extends JsonRules {

  def ExpressionRule: Rule1[Expression] = rule {
      Term ~ ws ~ zeroOrMore(operation ~ ws ~ Term ~ ws ~> ((op, expr) => PartialOperation(op, expr))) ~> ((left: Expression, others: Seq[PartialOperation]) => others.foldLeft(left)({
        case (expr, op) => createOperation(expr, op).getOrThrow() // TODO: Convert this to use fail()
      }))
    }
    //.recoverMerge { case failure => fail(failure.message) }

  def Term:Rule1[Expression] = rule {
    ws ~ Factor ~ ws ~ zeroOrMore(
      operation ~ ws ~ Factor ~ ws ~> ((op, expr) => PartialOperation(op,expr))
    ) ~> ((left:Expression, others:Seq[PartialOperation]) => others.foldLeft(left)({
      case (expr, op) => createOperation(expr,op).getOrThrow() // TODO: Convert this to use fail()
    }))
  }

  private def createOperation(left:Expression, op:PartialOperation): Result[Expression] = op.op match {
    case "+" => Success(ValueExpression(left, op.expr, Plus))
    case "-" => Success(ValueExpression(left, op.expr, Minus))
    case "/" => Success(ValueExpression(left, op.expr, Divide))
    case "*" => Success(ValueExpression(left, op.expr, Multiple))
    case "||" => Success(BooleanExpression(left, op.expr, Or))
    case "&&" => Success(BooleanExpression(left, op.expr, And))
    case ">" => Success(ComparaisonExpression(left, op.expr, GreaterThan))
    case "<" => Success(ComparaisonExpression(left, op.expr, LesserThan))
    case ">=" => Success(ComparaisonExpression(left, op.expr, GreaterOrEqual))
    case "<=" => Success(ComparaisonExpression(left, op.expr, LesserOrEqual))
    case "=" => Success(ComparaisonExpression(left, op.expr, Equals))
    case _ => Failure(s"unknown operation ${op.op}")
  }

  def Factor:Rule1[Expression] = rule {constant | conditionalVariableDefinition | variableMemberInner | variableName | Parens | UnaryMinus | UnaryNot }

  def Parens:Rule1[Expression] = rule { '(' ~ ws ~ ExpressionRule ~ ws ~ ')' ~> ((expr:Expression) => ParensExpression(expr)) }

  def UnaryMinus:Rule1[Expression] = rule { '-' ~ ExpressionRule ~> ((expr: Expression) => ValueExpression(NumberConstant(BigDecimal(-1)), expr, Multiple))}

  def UnaryNot:Rule1[Expression] = rule { '!' ~ ExpressionRule ~> ((expr: Expression) => BooleanUnaryExpression(expr, Not))}

  private def operation:Rule1[String] = rule {
    capture("+" | "-" | "/" | "*" | ">=" | "<=" | ">" | "<" | "=" | "||" | "&&")
  }

  def variableAlias : Rule1[VariableAliasing] = rule { openS  ~ zeroOrMore(" ") ~ variableAliasingDefinition ~ zeroOrMore(" ") ~ closeS
  }

  def varAliasKey: Rule1[VariableAliasing] = rule { &(openS) ~ variableAlias  }

  def variableAliasingDefinition:Rule1[VariableAliasing] = rule {
    "@" ~ charsKeyAST ~ zeroOrMore(' ')  ~ "=" ~ zeroOrMore(' ') ~ ExpressionRule ~>
      ((aKey:String, expression:Expression) => {
        VariableAliasing(VariableName(aKey.trim), expression)
      })
  }

  def variableName:Rule1[VariableName] = rule {
    charsKeyAST ~> ((name:String) => VariableName(name.trim))
  }

  def conditionalVariableDefinition:Rule1[VariableDefinition] = rule {
    variableName ~ ws ~ stringDefinition ~> ((variableName:VariableName, description:String) => VariableDefinition(variableName, Some(VariableTypeDefinition(YesNoType.name)), Some(description)))
  }

  def formatterDefinition:Rule1[FormatterDefinition] = rule {
    charsKeyAST ~ optional("(" ~ ws ~ parametersDefinition ~ ws ~ ")") ~> ((name:String, parameters:Option[Parameter]) => FormatterDefinition(name, parameters))
  }

  def variableDefinition:Rule1[VariableDefinition] = rule {
    optional(capture("#")) ~
      variableName ~
      optional(variableTypeDefinition) ~ ws ~
      optional("|" ~ ws ~ formatterDefinition)  ~ ws ~
      optional(stringDefinition) ~>
      ((prefix:Option[String], name:VariableName, optVarType:Option[(VariableTypeDefinition, Option[Parameter])], formatter:Option[FormatterDefinition], desc:Option[String]) => {
        val varTypeDefinition = optVarType.map({ case (variableType, _) => variableType})
        val optParams = optVarType.flatMap({case(_, ordered) => ordered})

        VariableDefinition(name, varTypeDefinition , desc.map(_.trim), formatter, prefix.isDefined, optParams)
      })
  }

  def variableTypeDefinition:Rule1[(VariableTypeDefinition, Option[Parameter])] = rule {
    ":" ~ variableType ~ optional("(" ~ ws ~ parametersDefinition ~ ws ~ ")") ~> ((varType:VariableTypeDefinition, params:Option[Parameter]) => (varType, params))
  }

  def variableType: Rule1[VariableTypeDefinition] = rule {
    capture(oneOrMore(keyChar)) ~ optional("<" ~ variableType ~ ">") ~> ((s: String, optTypeParameter:Option[VariableTypeDefinition]) => VariableTypeDefinition(s.trim, optTypeParameter))
  }

  def variable : Rule1[VariableDefinition] = rule {
    openS ~ zeroOrMore(" ") ~ variableDefinition ~ zeroOrMore(" ") ~ closeS
  }

  def variableMember: Rule1[VariableMember] = rule {
    openS ~ zeroOrMore(" ") ~ charsKeyAST ~ oneOrMore("." ~ charsKeyAST) ~ optional( ws ~ "|" ~ formatterDefinition) ~ closeS ~>((name:String, member:Seq[String], formatter:Option[FormatterDefinition]) => VariableMember(VariableName(name), member.map(_.trim), formatter))
  }

  def variableMemberInner: Rule1[VariableMember] = rule {
    charsKeyAST ~ oneOrMore("." ~ charsKeyAST) ~>((name:String, member:Seq[String]) => VariableMember(VariableName(name),member.map(_.trim), None))
  }

  def varKey: Rule1[VariableDefinition] = rule { &(openS) ~ variable }

  def varMemberKey: Rule1[VariableMember] = rule { &(openS) ~  variableMember}

  def parametersDefinition:Rule1[Parameter] = rule {
    parametersMapDefinition |
    (oneOrMore(ws ~ ExpressionRule ~ ws).separatedBy(",") ~> {
     s: Seq[Expression] =>
      s.toList match {
        // the typical match for Seq() triggers a compiler bug, so this is a workaround
        case head::Nil => OneValueParameter(head)
        case lst => ListParameter(lst)
      }
    })
  }

  def parametersMapDefinition:Rule1[Parameters] = rule {
    oneOrMore(ws ~ charsKeyAST ~ ws ~ ":" ~ ws ~ (MappingParameterEntry | parameterEntry) ~ ws ~> ((key,value) => key -> value))
      .separatedBy(";") ~> ((values:Seq[(String, Parameter)]) => Parameters(values))
  }

  def parameterEntry:Rule1[Parameter] = rule {
    oneOrMore(ExpressionRule).separatedBy(',') ~> ((n:Seq[Expression]) => n.toList match {
      case name::Nil => OneValueParameter(name)
      case names => ListParameter(names.map(v => v))
    })
  }

  def MappingParameterEntry:Rule1[MappingParameter] = rule {
    oneOrMore(MappingRule).separatedBy(',') ~> ((n:Seq[(VariableName, Expression)]) => {
      val mappingInternal = n.map({case (n,v) => n.name -> v}).toMap
      MappingParameter(mappingInternal)
    })
  }

  def MappingRule:Rule1[(VariableName, Expression)] = rule {
    ws ~ charsKeyAST ~ ws ~ "->" ~ ws ~ ExpressionRule ~ ws ~> ((name:String, expr:Expression) => (VariableName(name.trim), expr))
  }

  def constant:Rule1[Expression] = rule {
    numberDefinition ~> ((constant:BigDecimal) => NumberConstant(constant)) |
    stringDefinition ~> ((constant:String) => StringConstant(constant)) |
    jsonDefinition ~> ((json:Json) => JsonConstant(json.noSpaces))
  }
}
sealed trait Operation

sealed trait BooleanOperation
sealed trait BooleanUnaryOperation {
  def toString(expr:Expression):String
}

case object GreaterThan extends Operation {
  override def toString: String = ">"
}
case object LesserThan extends Operation {
  override def toString: String = "<"
}
case object GreaterOrEqual extends Operation {
  override def toString: String = ">="
}
case object LesserOrEqual extends Operation {
  override def toString: String = "<="
}
case object Equals extends Operation {
  override def toString: String = "="
}


case object And extends BooleanOperation {
  override def toString: String = "&&"
}
case object Or extends BooleanOperation {
  override def toString: String = "||"
}
case object Not extends BooleanUnaryOperation {
  override def toString(expr: Expression): String = "!(" + expr.toString + ")"
}

sealed trait ValueOperation

case object Plus extends ValueOperation {
  override def toString: String = "+"
}
case object Minus extends ValueOperation{
  override def toString: String = "-"
}
case object Multiple extends ValueOperation{
  override def toString: String = "*"
}
case object Divide extends ValueOperation{
  override def toString: String = "/"
}

case object Compare extends ValueOperation

object Parameter {
  implicit val parameterEq:Eq[Parameter] = Eq.fromUniversalEquals

  implicit val parameterEnc:Encoder[Parameter] = (a: Parameter) => {
    Json.obj(
      "name" -> Json.fromString(a.getClass.getSimpleName),
      "value" -> a.serialize
    )
  }
  implicit val parameterDec:Decoder[Parameter] = (c: HCursor) => {
    c.downField("name").as[String].flatMap(decodeParameter(_, c))
  }

  private def className[T](implicit classTag: ClassTag[T]):String = classTag.runtimeClass.getSimpleName

  private def decodeParameter(name: String, cursor: HCursor):Decoder.Result[Parameter] = {
    name match {
      case _ if name === className[OneValueParameter] =>
        cursor.downField("value").as[OneValueParameter]
      case _ if name === className[ListParameter] =>
        cursor.downField("value").as[ListParameter]
      case _ if name === className[Parameters] =>
        cursor.downField("value").as[Parameters]
      case _ if name === className[MappingParameter] =>
        cursor.downField("value").as[MappingParameter]
      case _ => Left(DecodingFailure(s"unknown parameter type $name", List()))
    }
  }
}

sealed trait Parameter extends OpenlawNativeValue {
  def serialize:Json
  def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]]
}
final case class OneValueParameter(expr:Expression) extends Parameter {
  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    expr.variables(executionResult)

  override def serialize: Json = this.asJson
}
final case class ListParameter(exprs:Seq[Expression]) extends Parameter {
  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    exprs.toList.map(_.variables(executionResult)).sequence.map(_.flatten.distinct)

  override def serialize: Json = this.asJson
}

final case class Parameters(parameterMap:Seq[(String, Parameter)]) extends Parameter {
  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    parameterMap.toList.map { case (_,param) => param.variables(executionResult) }.sequence.map(_.flatten.distinct)

  override def serialize: Json = this.asJson
}

final case class MappingParameter(mappingInternal: Map[String, Expression]) extends Parameter {
  def mapping:Map[VariableName, Expression] = mappingInternal.map({case (key,value) => VariableName(key) -> value})
  override def variables(executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    mapping.values.toList.map(_.variables(executionResult)).sequence.map(_.flatten.distinct)

  override def serialize: Json = this.asJson
}

object VariableTypeDefinition {
  implicit val variableTypeDefinitionEq:Eq[VariableTypeDefinition] = Eq.fromUniversalEquals
  implicit val variableTypeDefinitionEnc:Encoder[VariableTypeDefinition] = deriveEncoder[VariableTypeDefinition]
  implicit val variableTypeDefinitionDec:Decoder[VariableTypeDefinition] = deriveDecoder[VariableTypeDefinition]
}

case class VariableTypeDefinition(name:String, typeParameter:Option[VariableTypeDefinition] = None)

case class PartialOperation(op:String, expr:Expression)