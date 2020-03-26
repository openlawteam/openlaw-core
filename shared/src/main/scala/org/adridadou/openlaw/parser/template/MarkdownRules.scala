package org.adridadou.openlaw.parser.template

import org.parboiled2._

trait MarkdownRules extends GlobalRules {

  def loosenTextElement: Rule1[Seq[TextElement]] = rule {
    centeredLine | rightThreeQuartersLine | rightLine | pageBreak | sectionBreak | indentLine | loosenStrongWord | loosenEmWord | loosenUnderWord | textLoosen
  }

  def centeredLine: Rule1[Seq[TextElement]] = rule {
    capture(centered) ~ optional(ws) ~> ((_: String) => Seq(Centered))
  }

  def rightLine: Rule1[Seq[TextElement]] = rule {
    capture(right) ~ optional(ws) ~> ((_: String) => Seq(RightAlign))
  }

  def rightThreeQuartersLine: Rule1[Seq[TextElement]] = rule {
    capture(rightThreeQuarters) ~ optional(ws) ~> (
        (_: String) => Seq(RightThreeQuarters)
    )
  }

  def pageBreak: Rule1[Seq[TextElement]] = rule {
    capture(pagebreak) ~ optional(ws) ~> ((_: String) => Seq(PageBreak))
  }

  def sectionBreak: Rule1[Seq[TextElement]] = rule {
    capture(sectionbreak) ~ optional(ws) ~> ((_: String) => Seq(SectionBreak))
  }

  def indentLine: Rule1[Seq[TextElement]] = rule {
    capture(indent) ~ optional(ws) ~> ((_: String) => Seq(Indent))
  }

  def twoStar: Rule0 = rule(strong ~ !twoStar)
  def loosenTwoStarContents: Rule1[Seq[TextElement]] = rule {
    !twoStar ~ loosenTextElement
  }
  def loosenStrongWord: Rule1[Seq[TextElement]] = rule {
    twoStar ~ loosenTwoStarContents ~ twoStar ~> (
        (elems: Seq[TextElement]) => Seq(Strong) ++ elems ++ Seq(Strong)
    )
  }

  def oneStar: Rule0 = rule(em ~ !oneStar)
  def loosenOneStarcontents: Rule1[Seq[TextElement]] = rule {
    loosenStrongWord | (!oneStar ~ loosenTextElement)
  }
  def loosenEmWord: Rule1[Seq[TextElement]] = rule {
    oneStar ~ loosenOneStarcontents ~ oneStar ~> (
        (elems: Seq[TextElement]) => Seq(Em) ++ elems ++ Seq(Em)
    )
  }

  def underLines: Rule0 = rule(under ~ !underLines)
  def loosenUnderLinesContents: Rule1[Seq[TextElement]] = rule {
    loosenUnderWord | (!underLines ~ loosenTextElement)
  }
  def loosenUnderWord: Rule1[Seq[TextElement]] = rule {
    underLines ~ loosenUnderLinesContents ~ underLines ~> (
        (elems: Seq[TextElement]) => Seq(Under) ++ elems ++ Seq(Under)
    )
  }

  def textLoosen: Rule1[Seq[Text]] = rule {
    capture(loosenCharacters) ~> (
        (s: String) => Seq(Text(TextCleaning.dots(s)))
    )
  }

}
