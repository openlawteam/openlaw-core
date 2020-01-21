package org.adridadou.openlaw.parser.template

import cats.implicits._
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import org.adridadou.openlaw.result.Implicits.RichTry
import org.parboiled2._

object MarkdownParser {

  def createMarkdownParser(markdown:String):MarkdownParser = new MarkdownParser(markdown)

  def parseMarkdownOrThrow(markdown: String): List[TextElement] = parseMarkdown(markdown) match {
    case Success(seq) => seq
    case Failure(ex, message) => throw new RuntimeException(s"error while parsing the markdown: $message")
  }

  def parseMarkdown(markdown: String): Result[List[TextElement]] = {
    val compiler = createMarkdownParser(markdown)

    attempt(compiler.rootRule.run().toResult).flatten match {
      case Failure(parseError: ParseError, _) => Failure(compiler.formatError(parseError))
      case Failure(ex, message) => Failure(ex.getClass + ":" + message)
      case Success(result) => Success(result)
    }
  }
}
/**
  * Created by davidroon on 05.06.16.
  */
class MarkdownParser(val input: ParserInput) extends Parser with MarkdownRules {

  def rootRule: Rule1[List[TextElement]] = rule { zeroOrMore(loosenTextElement) ~ EOI ~> ((elems:Seq[Seq[TextElement]]) => elems.flatten.toList)}
}
