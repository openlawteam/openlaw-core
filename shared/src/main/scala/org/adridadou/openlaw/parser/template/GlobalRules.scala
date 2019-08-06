package org.adridadou.openlaw.parser.template

import org.parboiled2._

/**
  * Created by davidroon on 06.06.17.
  */
trait GlobalRules extends Parser {
  val openA = "<%"
  val closeA = "%>"

  val openB = "{{"
  val closeB = "}}"

  val openS = "[["
  val closeS = "]]"

  val openC = "<<"
  val closeC = ">>"

  val openCloseAnnotationHeader ="\'\'\'"
  val openCloseAnnotationNote = "\"\"\""

  val sectionChar = "^"
  val sectionBreak = "\b"

  val variableSectionChar= "=="

  val nl = "\n"
  val tabOrSpace = " \t"
  val pipe = "|"
  val em = "*"
  val strong = "**"
  val under = "__"
  val colons = "::"

  val indent = "\\indent"
  val pagebreak = "\\pagebreak"
  val centered = "\\centered"
  val right = "\\right"
  val rightThreeQuarters = "\\right-three-quarters"

  def ws:Rule0 = rule {zeroOrMore(" " | "\n" | "\t" | "\r") }
  def wsNoReturn:Rule0 = rule {zeroOrMore(" " | "\t" | "\r") }

  def quote: Rule0 = rule {"“" | "\"" | "”" | "'" }

  def loosenChar: Rule0 = rule { !(pipe | centered | rightThreeQuarters | right | pagebreak | indent | em | under | openCloseAnnotationHeader | openCloseAnnotationNote) ~  ANY }

  def normalChar: Rule0 = rule { !( pipe | centered | rightThreeQuarters | right | pagebreak | indent | em | under | openS | closeS | openB | closeB | openC | closeC | openA | closeA | sectionChar | sectionBreak | variableSectionChar | openCloseAnnotationHeader | openCloseAnnotationNote) ~  ANY }

  def normalCharNoReturn: Rule0 = rule { !( nl | pipe | centered | rightThreeQuarters | right | pagebreak | indent | em | under | openS | closeS | openB | closeB | openC | closeC | openA | closeA | sectionChar | sectionBreak | variableSectionChar | openCloseAnnotationHeader | openCloseAnnotationNote) ~  ANY }

  def commentsChar: Rule0 = rule {zeroOrMore(noneOf(nl))}

  def normalCharNoColons: Rule0 = rule { !(colons | pipe | centered | rightThreeQuarters | right | pagebreak | indent | em | under | openS | closeS | openB | closeB | openC | closeC | openA | closeA | sectionChar | sectionBreak | variableSectionChar | openCloseAnnotationHeader | openCloseAnnotationNote) ~  ANY }

  def characters: Rule0 = rule { oneOrMore(normalChar)  } // word

  def charactersNoReturn: Rule0 = rule { oneOrMore(normalCharNoReturn)  } // word

  def charactersNoColons: Rule0 = rule { oneOrMore(normalCharNoColons)  }

  def loosenCharacters: Rule0 = rule { oneOrMore(loosenChar)  } // word

  def firstKeyChar: Rule0 = rule { !(forbiddenChar | CharPredicate.Digit | " ") ~ ANY }

  def keyChar: Rule0 = rule { !forbiddenChar ~ ANY }

  def forbiddenChar: Rule0 =  rule {
    centered | rightThreeQuarters | right | pagebreak | indent | em | under | openS | closeS | openB | closeB | openC | closeC | sectionChar | sectionBreak | ":" | "|" | "&" | "@" | "#" | quote | "\n" | "," | "." | "->" | ">" | "<" | "=" | ")" | "(" | "+" | "-" | "*" | "/" | ";" | "!" | "{" | "}" | "[" | "]"
  }

  def charsKeyAST: Rule1[String] = rule { ws ~ capture(firstKeyChar ~ zeroOrMore(keyChar)) }

  def stringDefinition:Rule1[String] = rule {
    stringDefinition("\"") |
    stringDefinition("“","”")  |
    stringDefinition("'")
  }

  def stringDefinition(quoteChar:String):Rule1[String] = rule {
    &(quoteChar) ~ stringDefinitionInternal(quoteChar)
  }

  def stringDefinition(startQuoteChar:String, endQuoteChar:String):Rule1[String] = rule {
    &(startQuoteChar) ~ stringDefinitionInternal(startQuoteChar, endQuoteChar)
  }

  def stringDefinitionInternal(quoteChar:String):Rule1[String] = stringDefinitionInternal(quoteChar, quoteChar)

  def stringDefinitionInternal(startQuoteChar:String, endQuoteChar:String):Rule1[String] = rule {
    startQuoteChar ~ capture(zeroOrMore(!(startQuoteChar | endQuoteChar) ~ ANY)) ~ endQuoteChar
  }

  def numberDefinition:Rule1[BigDecimal] = rule {capture(oneOrMore(CharPredicate.Digit) ~ optional(("." | ",") ~ oneOrMore(CharPredicate.Digit))) ~>((str:String) => BigDecimal(str))}
}
