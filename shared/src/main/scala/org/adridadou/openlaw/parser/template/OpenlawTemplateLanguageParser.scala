package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.parser.template.variableTypes.{
  LargeTextType,
  TemplateType,
  TextType
}
import org.parboiled2._
import cats.implicits._
import VariableTypeDefinition._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

/**
  * Created by davidroon on 05.06.16.
  */
class OpenlawTemplateLanguageParser(
    val input: ParserInput
) extends Parser
    with BlockRules {

  def rootRule: Rule1[CompiledTemplate] = rule {
    optional(headerRule) ~ blockRule ~ EOI ~> (
        (
            optHeader: Option[TemplateHeader],
            block: Block
        ) => toCompiledTemplate(optHeader.getOrElse(TemplateHeader()), block)
    )
  }

  private def headerRule = rule {
    &("###") ~ headerPropertiesRule
  }

  private def headerPropertiesRule: Rule1[TemplateHeader] = rule {
    "##" ~ oneOrMore("#") ~ ws ~ oneOrMore(headerEntry) ~ ws ~ "##" ~ oneOrMore(
      "#"
    ) ~ ws ~> (
        (entries: Seq[(String, String)]) => TemplateHeader(entries.toMap)
    )
  }

  private def headerEntry: Rule1[(String, String)] = rule {
    headerKey ~ ":" ~ headerValue ~ ";" ~> (
        (
            key: String,
            value: String
        ) => key.trim -> value.trim
    )
  }

  private def headerKey: Rule1[String] = rule {
    capture(oneOrMore(!(";" | ":" | "###") ~ ANY))
  }

  private def headerValue: Rule1[String] = rule {
    capture(oneOrMore(!(";" | "###") ~ ANY))
  }

  private def toCompiledTemplate(
      header: TemplateHeader,
      block: Block
  ): CompiledTemplate = {
    getTemplate(header, block)
  }

  private def getTemplate(
      header: TemplateHeader,
      block: Block
  ): CompiledTemplate = CompiledAgreement(header = header, block = block)

  private def getVariables(block: Block): Map[String, VariableDefinition] =
    block
      .variables()
      .foldLeft(Map[String, VariableDefinition]())((varMap, variable) =>
        (varMap.get(variable.name.name), variable) match {
          case (None, _) => varMap ++ Map(variable.name.name -> variable)
          case (Some(VariableDefinition(name, Some(typeName), _, _, _, _)), _)
              if typeName === VariableTypeDefinition(TextType.name) =>
            varMap ++ Map(name.name -> variable)
          case (Some(VariableDefinition(name, Some(typeName), _, _, _, _)), _)
              if typeName === VariableTypeDefinition(LargeTextType.name) =>
            varMap ++ Map(name.name -> variable)
          case (Some(v), _)
              if v.description.isEmpty && variable.description.isEmpty =>
            varMap ++ Map(
              variable.name.name -> v.copy(description = variable.description)
            )
          case (
              Some(VariableDefinition(_, Some(typeName), _, _, _, _)),
              VariableDefinition(_, Some(variableTypeName), _, _, _, _)
              ) if typeName.name =!= variableTypeName.name =>
            throw new RuntimeException(
              "type mismatch for variable " + variable.name + ". The variable was defined as " + typeName + " and then again as " + variableTypeName
            )
          case _ => varMap
        }
      )
}

object TemplateHeader {
  implicit val templateHeaderEnc: Encoder[TemplateHeader] = deriveEncoder
  implicit val templateHeaderDec: Decoder[TemplateHeader] = deriveDecoder
}

final case class TemplateHeader(values: Map[String, String] = Map()) {
  def shouldShowTitle: Boolean = {
    values.get("show title").exists(_.toBoolean) ||
    values.get("title").exists(_ === "show")
  }
}
