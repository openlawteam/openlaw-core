package org.adridadou.openlaw.parser.template.printers

import java.time.Clock

import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.TemplateParameters
import org.adridadou.openlaw.vm.OpenlawExecutionEngine
import org.adridadou.openlaw.parser.template.printers.XHtmlAgreementPrinter.FragsPrinter
import org.scalatest._

class XHtmlAgreementPrinterSpec extends FlatSpec with Matchers with EitherValues {

  private val clock = Clock.systemUTC
  private val service = new OpenlawTemplateLanguageParserService(clock)
  private val engine = new OpenlawExecutionEngine

  private def structureAgreement(text:String, p:Map[String, String] = Map(), templates:Map[TemplateSourceIdentifier, CompiledTemplate] = Map()):Either[String, StructuredAgreement] = compiledTemplate(text).flatMap({
    case agreement:CompiledAgreement =>
      val params = p.map({case (k,v) => VariableName(k) -> v})
      engine.execute(agreement, TemplateParameters(params), templates).map(agreement.structuredMainTemplate)
    case _ =>
      Left("was expecting agreement")
  })

  private def compiledTemplate(text:String):Either[String, CompiledTemplate] = service.compileTemplate(text)

  private def compiledAgreement(text:String):Either[String, CompiledAgreement] = compiledTemplate(text) match {
    case Right(agreement:CompiledAgreement) => Right(agreement)
    case Right(_) => Left("was expecting agreement")
    case Left(ex) => Left(ex)
  }

  private def forReview(text:String, params:Map[String, String] = Map(), paragraphs:ParagraphEdits = ParagraphEdits(Map())):Either[String, String] =
    structureAgreement(text,params).map(service.forReview(_, paragraphs))
  private def forPreview(text:String, params:Map[String, String] = Map(), paragraphs:ParagraphEdits = ParagraphEdits(Map())):Either[String, String] =
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

    val agreement = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.right.value.paragraphs).print
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

    val agreement = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.right.value.paragraphs).print
    html shouldBe """<p class="no-section">This is a test<br /></p><p class="no-section">Another line</p>"""
  }

  it should "handle right aligned section" in {
    val text = """\right **[[Company Name | Uppercase]]**"""
    val agreement = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.right.value.paragraphs).print
    html shouldBe """<p class="no-section align-right"> <strong>[[Company Name]]</strong></p>"""
  }

  it should "handle right 3/4 aligned section" in {
    val text = """\right-three-quarters **[[Company Name | Uppercase]]**"""
    val agreement = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.right.value.paragraphs).print
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

    val agreement = structureAgreement(text)
    val html = XHtmlAgreementPrinter(false).printParagraphs(agreement.right.value.paragraphs).print
    html shouldBe """<p class="no-section align-center"> <strong>BYLAWS</strong><br /> <strong>OF</strong><br /> <strong>[[Company Name]]</strong><br /> (A DELAWARE CORPORATION)<br /></p><ul class="list-lvl-1"><li><p>1.  <strong>Offices</strong></p><ul class="list-lvl-2"><li><p>(a)  <strong>Registered Office</strong>.  The registered office of the corporation in the State of Delaware shall be [[Registered Agent Address]], and the name of the registered agent of the corporation in the State of Delaware at such address is [[Registered Agent Name]].<br /></p></li></ul></li></ul>"""

  }
}
