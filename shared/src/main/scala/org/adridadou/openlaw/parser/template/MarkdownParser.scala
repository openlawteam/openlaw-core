package org.adridadou.openlaw.parser.template

import org.parboiled2._

object MarkdownParser {

  def createMarkdownParser(markdown:String):MarkdownParser = new MarkdownParser(markdown)

  def parseMarkdownOrThrow(markdown: String): Seq[TextElement] = parseMarkdown(markdown) match {
    case Left(ex) => throw new RuntimeException("error while parsing the markdown:" + ex)
    case Right(seq) => seq
  }

  def parseMarkdown(markdown: String): Either[String, Seq[TextElement]] = {
    val compiler = createMarkdownParser(markdown)

    compiler.rootRule.run().toEither match {
      case Left(parseError: ParseError) => Left(compiler.formatError(parseError))
      case Left(ex) => Left(ex.getClass + ":" + ex.getMessage)
      case Right(result) => Right(result)
    }
  }
}
/**
  * Created by davidroon on 05.06.16.
  */
class MarkdownParser(val input: ParserInput) extends Parser with MarkdownRules {

  def rootRule: Rule1[Seq[TextElement]] = rule { zeroOrMore(loosenTextElement) ~ EOI ~>((elems:Seq[Seq[TextElement]]) => elems.flatten)}
}
