package org.adridadou.openlaw.parser.template

import org.parboiled2._

trait MarkdownRules extends GlobalRules {

  def loosenTextElement: Rule1[Seq[TextElement]] = rule {
    centeredLine | rightThreeQuartersLine | rightLine | pageBreak | indentLine | loosenStrongWord | loosenEmWord | loosenUnderWord | textLoosen
  }

  def centeredLine: Rule1[Seq[TextElement]] = rule {
    capture(centered) ~> ((_: String) => Seq(Centered))
  }

  def rightLine: Rule1[Seq[TextElement]] = rule {
    capture(right) ~> ((_: String) => Seq(RightAlign))
  }

  def rightThreeQuartersLine: Rule1[Seq[TextElement]] = rule {
    capture(rightThreeQuarters) ~> ((_: String) => Seq(RightThreeQuarters))
  }

  def pageBreak: Rule1[Seq[TextElement]] = rule {
    capture(pagebreak) ~ "\n" ~> ((_: String) => Seq(PageBreak))
  }

  def indentLine: Rule1[Seq[TextElement]] = rule {
    capture(indent) ~> ((_: String) => Seq(Indent))
  }

  def twoStar: Rule0 = rule(strong ~ !twoStar)
  def loosenTwoStarContents: Rule1[Seq[TextElement]] = rule { !twoStar ~ loosenTextElement }
  def loosenStrongWord: Rule1[Seq[TextElement]] = rule { twoStar ~ loosenTwoStarContents ~ twoStar ~> ((elems:Seq[TextElement]) => Seq(Strong) ++ elems ++ Seq(Strong)) }

  def oneStar: Rule0 = rule(em ~ !oneStar)
  def loosenOneStarcontents: Rule1[Seq[TextElement]] = rule { loosenStrongWord | (!oneStar ~ loosenTextElement) }
  def loosenEmWord: Rule1[Seq[TextElement]] = rule { oneStar ~ loosenOneStarcontents ~ oneStar ~> ((elems:Seq[TextElement]) => Seq(Em) ++ elems ++ Seq(Em)) }

  def underLines: Rule0 = rule(under ~ !underLines)
  def loosenUnderLinescontents: Rule1[Seq[TextElement]] = rule { loosenUnderWord | (!underLines ~ loosenTextElement) }
  def loosenUnderWord: Rule1[Seq[TextElement]] = rule { underLines ~ loosenUnderLinescontents ~ underLines ~> ((elems:Seq[TextElement]) => Seq(Em) ++ elems ++ Seq(Em)) }

  def textLoosen: Rule1[Seq[Text]] = rule {
    capture(loosenCharacters) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

}
