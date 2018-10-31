package org.adridadou.openlaw.parser.template

import org.parboiled2._

trait MarkdownRules extends GlobalRules {

  def loosenTextElement: Rule1[Seq[TextElement]] = rule {
    centeredLine | pageBreak | loosenStrongWord | loosenEmWord | textLoosen
  }

  def centeredLine: Rule1[Seq[TextElement]] = rule {
    capture(centered) ~> ((s: String) => Seq(Centered))
  }

  def pageBreak: Rule1[Seq[TextElement]] = rule {
    capture(pagebreak) ~ "\n" ~> ((s: String) => Seq(PageBreak))
  }

  def twoStar: Rule0 = rule(strong ~ !twoStar)
  def loosenTwoStarContents: Rule1[Seq[TextElement]] = rule { !twoStar ~ loosenTextElement }
  def loosenStrongWord: Rule1[Seq[TextElement]] = rule { twoStar ~ loosenTwoStarContents ~ twoStar ~> ((elems:Seq[TextElement]) => Seq(Strong) ++ elems ++ Seq(Strong)) }

  def oneStar: Rule0 = rule(em ~ !oneStar)
  def loosenOneStarcontents: Rule1[Seq[TextElement]] = rule { loosenStrongWord | (!oneStar ~ loosenTextElement) }
  def loosenEmWord: Rule1[Seq[TextElement]] = rule { oneStar ~ loosenOneStarcontents ~ oneStar ~> ((elems:Seq[TextElement]) => Seq(Em) ++ elems ++ Seq(Em)) }

  def textLoosen: Rule1[Seq[Text]] = rule {
    capture(loosenCharacters) ~> ((s: String) => Seq(Text(TextCleaning.dots(s))))
  }

}
