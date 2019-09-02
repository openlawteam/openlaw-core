package org.adridadou.openlaw.parser.template.printers

import java.time.Clock

import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.result.Implicits.failureCause2Exception
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.{TemplateParameters, TemplateTitle}
import org.adridadou.openlaw.vm.OpenlawExecutionEngine
import org.adridadou.openlaw.parser.template.printers.XHtmlAgreementPrinter.FragsPrinter
import org.adridadou.openlaw.result.{Failure, Result}
import org.scalatest._

class XHtmlAgreementPrinterSpec extends FlatSpec with Matchers with EitherValues {

  private val clock = Clock.systemUTC
  private val service = new OpenlawTemplateLanguageParserService(clock)
  private val engine = new OpenlawExecutionEngine

  private def structureAgreement(text:String, p:Map[String, String] = Map(), templates:Map[TemplateSourceIdentifier, CompiledTemplate] = Map()):Result[StructuredAgreement] = compiledTemplate(text).flatMap({
    case agreement:CompiledAgreement =>
      val params = p.map({case (k,v) => VariableName(k) -> v})
      engine.execute(agreement, TemplateParameters(params), templates).flatMap(agreement.structuredMainTemplate)
    case _ =>
      Failure("was expecting agreement")
  })

  private def compiledTemplate(text:String): Result[CompiledTemplate] = service.compileTemplate(text)

  private def forReview(text:String, params:Map[String, String] = Map(), paragraphs:ParagraphEdits = ParagraphEdits(Map())):Result[String] =
    structureAgreement(text,params).map(service.forReview(_, paragraphs))
  private def forPreview(text:String, params:Map[String, String] = Map(), paragraphs:ParagraphEdits = ParagraphEdits(Map())):Result[String] =
    structureAgreement(text,params).map(service.forPreview(_, paragraphs))

  private def resultShouldBe(result:Either[String, String], expected:String): Unit = result match {
    case Right(actual) if actual === expected=>
    case Right(actual) => throw new RuntimeException(s"$actual should be $expected")
    case Left(ex) => throw new RuntimeException(ex)
  }

  "The XHtmlAgreementPrinter" should "print a simple agreement" in {
    val text= """[[Id:Identity]]
      |
      |^ **Section 1**
      |
      |This is a test.
      |
      |^ Section 2.
      |
      |This is a test.
      |
      |^^ Section 2.a
      |
      |This is a test.
      |
      |^^ Section 2.b
      |
      |This is a test.
      |
      |^ Section 3.
      |
      |This is a test.
      |""".stripMargin

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.paragraphs).print
    html shouldBe
      """<p class="no-section"></p><ul class="list-lvl-1"><li><p>1.  <strong>Section 1</strong></p><p>This is a test.</p></li><li><p>2.  Section 2.</p><p>This is a test.</p><ul class="list-lvl-2"><li><p>(a)  Section 2.a</p><p>This is a test.</p></li><li><p>(b)  Section 2.b</p><p>This is a test.</p></li></ul></li><li><p>3.  Section 3.</p><p>This is a test.<br /></p></li></ul>"""
  }

  it should "properly parse text with newlines" in {
    val text = """This is a test
      |
      |
      |Another line
      |
      |""".stripMargin

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.paragraphs).print
    html shouldBe """<p class="no-section">This is a test<br /></p><p class="no-section">Another line</p>"""
  }

  it should "print a template title" in {
    val html = XHtmlAgreementPrinter(false).printFragments(List(Title(TemplateTitle("title1")))).print
    html shouldBe """<h1 class="signature-title">title1</h1>"""
  }

  it should "handle right aligned section" in {
    val text = """\right **[[Company Name | Uppercase]]**"""
    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.paragraphs).print
    html shouldBe """<p class="no-section align-right"> <strong>[[Company Name]]</strong></p>"""
  }

  it should "handle right aligned underlined section" in {
    val text = """\right __[[Company Name | Uppercase]]__"""
    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.paragraphs).print
    html shouldBe """<p class="no-section align-right"> <u>[[Company Name]]</u></p>"""
  }

  it should "handle right 3/4 aligned section" in {
    val text = """\right-three-quarters **[[Company Name | Uppercase]]**"""
    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.paragraphs).print
    html shouldBe """<p class="no-section align-right-three-quarters"> <strong>[[Company Name]]</strong></p>"""
  }

  it should "handle agreements with multiple centered sections" in {
    val text = """\centered **BYLAWS**
                 |\centered **OF**
                 |\centered **[[Company Name | Uppercase]]**
                 |\centered (A DELAWARE CORPORATION)
                 |
                 |
                 |^ **Offices**
                 |
                 |^^ **Registered Office**.  The registered office of the corporation in the State of Delaware shall be [[Registered Agent Address: Address]], and the name of the registered agent of the corporation in the State of Delaware at such address is [[Registered Agent Name]].
                 |""".stripMargin

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.paragraphs).print
    html shouldBe """<p class="no-section align-center"> <strong>BYLAWS</strong><br /> <strong>OF</strong><br /> <strong>[[Company Name]]</strong><br /> (A DELAWARE CORPORATION)<br /></p><ul class="list-lvl-1"><li><p>1.  <strong>Offices</strong></p><ul class="list-lvl-2"><li><p>(a)  <strong>Registered Office</strong>.  The registered office of the corporation in the State of Delaware shall be [[Registered Agent Address]], and the name of the registered agent of the corporation in the State of Delaware at such address is [[Registered Agent Name]].<br /></p></li></ul></li></ul>"""

  }

  it should "handle tables with variables in multiple rows" in {
    val text = """This is | a test.
      || head1 | head2 | head3 |
      || ----- | ----- | ----- |
      || [[var1]] | val12 | val13 |
      || val21 | val22 | val23 |
      || [[var31]] | val32 | val33 |
      || val41 | val42 | val43 |
      |This is a test.""".stripMargin

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(true).printParagraphs(agreement.paragraphs).print
    html shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section">This is | a test.<br /><table class="markdown-table"><tr class="markdown-table-row"><th class="markdown-table-header">head1</th><th class="markdown-table-header">head2</th><th class="markdown-table-header">head3</th></tr><tr class="markdown-table-row"><td class="markdown-table-data"><span class="markdown-variable markdown-variable-var1">[[var1]]</span></td><td class="markdown-table-data">val12</td><td class="markdown-table-data">val13</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">val21</td><td class="markdown-table-data">val22</td><td class="markdown-table-data">val23</td></tr><tr class="markdown-table-row"><td class="markdown-table-data"><span class="markdown-variable markdown-variable-var31">[[var31]]</span></td><td class="markdown-table-data">val32</td><td class="markdown-table-data">val33</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">val41</td><td class="markdown-table-data">val42</td><td class="markdown-table-data">val43</td></tr></table>This is a test.</p></div>"""
  }

  it should "handle table with text with multiple words" in {
    val text = """
      || head1 | head2 |
      || ----- | ----- |
      || test test | test |
      || test | test |
      |""".stripMargin

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(true).printParagraphs(agreement.paragraphs).print
    html shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section"><br /><table class="markdown-table"><tr class="markdown-table-row"><th class="markdown-table-header">head1</th><th class="markdown-table-header">head2</th></tr><tr class="markdown-table-row"><td class="markdown-table-data">test test</td><td class="markdown-table-data">test</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">test</td><td class="markdown-table-data">test</td></tr></table></p></div>"""
  }

  it should "handle variables with text prefixes" in {
    val text = "$[[var31]]"

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(true).printParagraphs(agreement.paragraphs).print
    html shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section">$<span class="markdown-variable markdown-variable-var31">[[var31]]</span></p></div>"""
  }

  it should "handle table with a variables with text prefixes" in {
    val text = """
      || head1 | head2 |
      || ----- | ----- |
      || $[[var31]] | test |
      || test | test |
      |""".stripMargin

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(true).printParagraphs(agreement.paragraphs).print
    html shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section"><br /><table class="markdown-table"><tr class="markdown-table-row"><th class="markdown-table-header">head1</th><th class="markdown-table-header">head2</th></tr><tr class="markdown-table-row"><td class="markdown-table-data">$<span class="markdown-variable markdown-variable-var31">[[var31]]</span></td><td class="markdown-table-data">test</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">test</td><td class="markdown-table-data">test</td></tr></table></p></div>"""
  }

  it should "handle tables with variables with text prefixes" in {
    val text = """This is | a test.
      || head1 | head2 | head3 |
      || ----- | ----- | ----- |
      || [[var1]] | val12 | val13 |
      || val21 | val22 | val23 |
      || $[[var31]] | val32 | val33 |
      || val41 | val42 | val43 |
      |This is a test.""".stripMargin

    val Right(agreement) = structureAgreement(text)
    val html = XHtmlAgreementPrinter(true).printParagraphs(agreement.paragraphs).print
    html shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section">This is | a test.<br /><table class="markdown-table"><tr class="markdown-table-row"><th class="markdown-table-header">head1</th><th class="markdown-table-header">head2</th><th class="markdown-table-header">head3</th></tr><tr class="markdown-table-row"><td class="markdown-table-data"><span class="markdown-variable markdown-variable-var1">[[var1]]</span></td><td class="markdown-table-data">val12</td><td class="markdown-table-data">val13</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">val21</td><td class="markdown-table-data">val22</td><td class="markdown-table-data">val23</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">$<span class="markdown-variable markdown-variable-var31">[[var31]]</span></td><td class="markdown-table-data">val32</td><td class="markdown-table-data">val33</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">val41</td><td class="markdown-table-data">val42</td><td class="markdown-table-data">val43</td></tr></table>This is a test.</p></div>"""
  }

  it should "handle conditional block depth properly to have proper highlighting" in {
    val text = """before the conditional
    |
    | {{ if me => in the conditional }} after the conditional
    | this too
    |
    | yet another paragraph after the conditional""".stripMargin

    val Right(agreement) = structureAgreement(text, Map("if me" -> "true"))
    val html = service.forPreview(agreement, ParagraphEdits())
    html shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section">before the conditional<br /></p></div><div class="openlaw-paragraph paragraph-2"><p class="no-section"><span class="markdown-conditional-block">in the conditional </span> after the conditional<br /> this too<br /></p></div><div class="openlaw-paragraph paragraph-3"><p class="no-section">yet another paragraph after the conditional</p></div>"""
  }

  it should "not highlight things after a conditional" in {
    val text = """before the conditional
        |
        |{{if me => in the conditional }}
        |
        |after the conditional
        |this too
        |
        |
        |yet another paragraph after the conditional""".stripMargin

    val Right(template) = service.compileTemplate(text)
    engine.execute(template, TemplateParameters("if me" -> "true"), Map()) match {
      case Right(result) =>
        val text = service.forPreview(result.agreements.head, ParagraphEdits())
        text shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section">before the conditional</p></div><div class="openlaw-paragraph paragraph-2"><p class="no-section"><span class="markdown-conditional-block">in the conditional </span></p></div><div class="openlaw-paragraph paragraph-3"><p class="no-section">after the conditional<br />this too<br /></p></div><div class="openlaw-paragraph paragraph-4"><p class="no-section">yet another paragraph after the conditional</p></div>"""
      case Left(ex) => fail(ex)
    }
  }
}
