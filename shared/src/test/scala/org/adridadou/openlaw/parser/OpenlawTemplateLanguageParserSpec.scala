package org.adridadou.openlaw.parser

import java.time.{Clock, LocalDateTime, ZoneOffset}

import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.TemplateParameters
import org.adridadou.openlaw.vm.OpenlawExecutionEngine
import org.scalatest._

/**
  * Created by davidroon on 05.05.17.
  */
class OpenlawTemplateLanguageParserSpec extends FlatSpec with Matchers with EitherValues {

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

  "Markdown parser service" should "handle tables" in {
    val text=
      """| head1 | head2 | head3 |
    || ----- | ----- | ----- |
    || val11 | val12 | val13 |
    || val21 | val22 | val23 |
    |""".stripMargin

    val template = service.compileTemplate(text).right.value
    template shouldBe a [CompiledAgreement]
    val agreement = template.asInstanceOf[CompiledAgreement]
    val tableElement = structureAgreement(text).map(_.paragraphs(0).elements.head).right.value
    tableElement shouldBe a [TableElement]
    tableElement.asInstanceOf[TableElement].header should contain inOrderOnly (Seq(FreeText(Text("head1"))), Seq(FreeText(Text("head2"))), Seq(FreeText(Text("head3"))))

    resultShouldBe(forReview(text), """<p class="no-section"><table class="markdown-table"><tr class="markdown-table-row"><th class="markdown-table-header">head1</th><th class="markdown-table-header">head2</th><th class="markdown-table-header">head3</th></tr><tr class="markdown-table-row"><td class="markdown-table-data">val11</td><td class="markdown-table-data">val12</td><td class="markdown-table-data">val13</td></tr><tr class="markdown-table-row"><td class="markdown-table-data">val21</td><td class="markdown-table-data">val22</td><td class="markdown-table-data">val23</td></tr></table></p>""")
  }

  it should "handle tables following other elements" in {
    val text=
      """This is a test.
    || head1 | head2 | head3 |
    || ----- | ----- | ----- |
    || val11 | val12 | val13 |
    || val21 | val22 | val23 |
    |""".stripMargin

    val structure = structureAgreement(text)
    structure.map(_.paragraphs(0).elements.apply(1)).right.value shouldBe a [TableElement]
   }

  it should "handle tables preceeding other elements" in {
    val text=
      """|| head1 | head2 | head3 |
    || ----- | ----- | ----- |
    || val11 | val12 | val13 |
    || val21 | val22 | val23 |
    |This is a test.""".stripMargin

    val structure = structureAgreement(text)
    structureAgreement(text).map(_.paragraphs(0).elements.head).right.value shouldBe a [TableElement]
   }

   it should "handle tables mixed with other elements with pipes" in {
    val text=
      """This is | a test.
    || head1 | head2 | head3 |
    || ----- | ----- | ----- |
    || val11 | val12 | val13 |
    || val21 | val22 | val23 |
    |This is a test.""".stripMargin


    val structure = structureAgreement(text)
    structure.map(_.paragraphs(0).elements.apply(3)).right.value shouldBe a [TableElement]
   }

  it should "handle tables with variables in cells" in {
    val text=
      """This is | a test.
    || head1 | head2 | head3 |
    || ----- | ----- | ----- |
    || [[var1]] | val12 | val13 |
    || val21 | val22 | val23 |
    |This is a test.""".stripMargin

    val tableElement = structureAgreement(text).map(_.paragraphs(0).elements.apply(3)).right.value.asInstanceOf[TableElement]
    tableElement.rows.head.head.head shouldBe a [VariableElement]
   }

  it should "handle tables with conditionals in cells" in {
    val text=
      """This is | a test.
    || head1 | head2 | head3 |
    || ----- | ----- | ----- |
    || {{conditional1 "Question" => Question text}} | val12 | val13 |
    || val21 | val22 | val23 |
    |This is a test.""".stripMargin

    val tableElement = structureAgreement(text, Map("conditional1" -> "true")).map(_.paragraphs(0).elements.apply(3)).right.value.asInstanceOf[TableElement]
    tableElement.rows.head.head.head shouldBe a [ConditionalStart]
   }

  it should "parse and replace each variable with its value" in {

    val clauseText = "This is my clause. [[contractor]]. And I am born in [[contractorBirthdate]]"


    resultShouldBe(forReview(clauseText, Map(
      "contractor" -> "My contractor name",
      "contractorBirthdate" -> "January 13th 1983"
    )), """<p class="no-section">This is my clause. My contractor name. And I am born in January 13th 1983</p>""")
  }

  it should "compile the document and the compiled version can be then parsed" in {
    val clauseText = "This is my clause. [[contractor]]. And I am born in [[contractorBirthdate]]"
    val parameters = Map(
      "contractor" -> "My contractor name",
      "contractorBirthdate" -> "January 13th 1983")


    resultShouldBe(forReview(clauseText, parameters), """<p class="no-section">This is my clause. My contractor name. And I am born in January 13th 1983</p>""")
  }

  it should "be able to extract the variable definitions" in {

    val clauseText = "This is my clause. [[contractor \"the contractor who is going to do the job\"]]. And I am born in [[contractorBirthdate:Date \"The birthdate of the contractor\"]]"

    compiledAgreement(clauseText) match {
      case Right(compiledVersion) =>
        val variables = compiledVersion.block.variables()
        variables(0) shouldBe VariableDefinition(VariableName("contractor"), None, Some("the contractor who is going to do the job"), None)
        variables(1) shouldBe VariableDefinition(VariableName("contractorBirthdate"), Some(VariableTypeDefinition(DateType.name)), Some("The birthdate of the contractor"), None)
      case Left(ex) => fail(ex)
    }
  }

  it should "handle conditional blocks" in {

    val clauseText = """This is my clause. [[contractor:Text "the contractor who is going to do the job"]]. {{shouldShowBirthdate "Should we show the birthdate?" => And I am born in [[contractorBirthdate "The birthdate of the contractor"]]}}"""

    resultShouldBe(forReview(clauseText, Map(
      "contractor" -> "David Roon",
      "shouldShowBirthdate" -> "true",
      "contractorBirthdate" -> "01.13.1983"
    )) , """<p class="no-section">This is my clause. David Roon. And I am born in 01.13.1983</p>""")

    resultShouldBe(forReview(clauseText, Map(
      "contractor" -> "David Roon",
      "shouldShowBirthdate" -> "false"
    )), """<p class="no-section">This is my clause. David Roon. </p>""")
  }

  it should "do post processing for lists" in {
    val text =
      """a small title
        |
        |^this is a first element
        |^this is a second element
        |^^this is a first sub element
        |^^^this is a first sub sub element
        |^^^this is a second sub sub element
        |^this is a third element
        |^^this is yet another sub element
      """.stripMargin

    val text2 =
      """<div class="openlaw-paragraph paragraph-1"><p class="no-section">a small title</p></div><ul class="list-lvl-1"><li><div class="openlaw-paragraph paragraph-2"><p>1. this is a first element<br /></p></div></li><li><div class="openlaw-paragraph paragraph-3"><p>2. this is a second element<br /></p></div><ul class="list-lvl-2"><li><div class="openlaw-paragraph paragraph-4"><p>(a) this is a first sub element<br /></p></div><ul class="list-lvl-3"><li><div class="openlaw-paragraph paragraph-5"><p>(i) this is a first sub sub element<br /></p></div></li><li><div class="openlaw-paragraph paragraph-6"><p>(ii) this is a second sub sub element<br /></p></div></li></ul></li></ul></li><li><div class="openlaw-paragraph paragraph-7"><p>3. this is a third element<br /></p></div><ul class="list-lvl-2"><li><div class="openlaw-paragraph paragraph-8"><p>(a) this is yet another sub element<br />      </p></div></li></ul></li></ul>""".stripMargin

    val result = forPreview(text)
    resultShouldBe(result, text2)
  }

  it should "close li elements properly" in {
    val text =
      """[[Id:Identity]]
        |
        |^ **Services**. This is a test.
        |""".stripMargin

    val text2 =
      """<p class="no-section"></p><ul class="list-lvl-1"><li><p>1.  <strong>Services</strong>. This is a test.<br /></p></li></ul>"""

    val text3 =
      """<div class="openlaw-paragraph paragraph-1"><p class="no-section"><span class="markdown-variable markdown-variable-Id"></span></p></div><ul class="list-lvl-1"><li><div class="openlaw-paragraph paragraph-2"><p>1.  <strong>Services</strong>. This is a test.<br /></p></div></li></ul>"""

    resultShouldBe(forReview(text), text2)
    resultShouldBe(forPreview(text), text3)
  }

  it should "do post processing for lists on preview too (with paragraphs)" in {
    val text =
      """
        |a small title
        |^this is a first element
        |^this is a second element
        |^^this is a first sub element
        |^^^this is a first sub sub element
        |^^^^this is a first sub sub sub element
        |^^^^this is a second sub sub sub element
        |^^^this is a second sub sub element
        |^this is a third element
        |^^this is yet another sub element
      """.stripMargin

    val text2 =
      """<div class="openlaw-paragraph paragraph-1"><p class="no-section"><br />a small title<br /></p></div><ul class="list-lvl-1"><li><div class="openlaw-paragraph paragraph-2"><p>1. this is a first element<br /></p></div></li><li><div class="openlaw-paragraph paragraph-3"><p>2. this is a second element<br /></p></div><ul class="list-lvl-2"><li><div class="openlaw-paragraph paragraph-4"><p>(a) this is a first sub element<br /></p></div><ul class="list-lvl-3"><li><div class="openlaw-paragraph paragraph-5"><p>(i) this is a first sub sub element<br /></p></div><ul class="list-lvl-4"><li><div class="openlaw-paragraph paragraph-6"><p>(1) this is a first sub sub sub element<br /></p></div></li><li><div class="openlaw-paragraph paragraph-7"><p>(2) this is a second sub sub sub element<br /></p></div></li></ul></li><li><div class="openlaw-paragraph paragraph-8"><p>(ii) this is a second sub sub element<br /></p></div></li></ul></li></ul></li><li><div class="openlaw-paragraph paragraph-9"><p>3. this is a third element<br /></p></div><ul class="list-lvl-2"><li><div class="openlaw-paragraph paragraph-10"><p>(a) this is yet another sub element<br />      </p></div></li></ul></li></ul>""".stripMargin

    val result = forPreview(text)
    resultShouldBe(result, text2)
  }


  it should "parse for smart contract calls" in {
    val text = """
      |[[Var1:Text]]
      |[[Var2:Text]]
      |[[Var3:Text]]
      |[[My Contract Call:EthereumCall(
      |contract:"0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe";
      |interface:'ipfs:5ihruiherg34893zf';
      |startDate: '2018-12-12 00:00:00';
      |function:'callFunction';
      |arguments:Var1,Var2,Var3;
      |repeatEvery:'1 minute 12 seconds')]]
    """.stripMargin

    structureAgreement(text) match {
      case Right(structuredAgreement) =>
        structuredAgreement.executionResult.getVariables(EthereumCallType).size shouldBe 1
        val allActions = structuredAgreement.executionResult.allActions()
        allActions.size shouldBe 1

        val call = structuredAgreement.executionResult.getVariableValues[EthereumSmartContractCall](EthereumCallType).head
        call.address.asInstanceOf[StringConstant].value shouldBe "0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe"
        call.arguments.map(_.toString) shouldBe List("Var1","Var2","Var3")
        call.metadata.asInstanceOf[StringConstant].value shouldBe "ipfs:5ihruiherg34893zf"
      case Left(ex) => fail(ex)
    }
  }

  it should "be able to have pipe characters" in {
    val text = "This is a | test."
    resultShouldBe(forReview(text, Map("Var" -> "hello world")), """<p class="no-section">This is a | test.</p>""")
  }

  it should "be able to emphasize variables" in {
    val text = "* [[Var]] * ** [[Var]] ** *** [[Var]] ***"
    resultShouldBe(forReview(text, Map("Var" -> "hello world")), """<p class="no-section"><em> hello world </em> <strong> hello world </strong> <strong><em> hello world </em></strong></p>""")
  }

  it should "be able to override section symbols" in {
    resultShouldBe(forReview("^ Section 1", Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'Decimal')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'Fake')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'LowerLetter')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>a.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'UpperLetter')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>A.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'LowerRoman')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>i.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'UpperRoman')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>I.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'Hide')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>  Section 1</p></li></ul>""")
  }

  it should "be able to override section formats" in {
    resultShouldBe(forReview("^ Section 1", Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(format: 'Period')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(format: 'Parens')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>(1)  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(format: 'RightParen')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>1)  Section 1</p></li></ul>""")
  }

  it should "be able to reference sections" in {
    val text =
      """^ Section 1
        |^^ Section 1.a
        |^^^(s1ai) Section 1.a.i
        |
        |[[s1ai]]
      """.stripMargin
    resultShouldBe(forReview(text, Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1<br /></p><ul class="list-lvl-2"><li><p>(a)  Section 1.a<br /></p><ul class="list-lvl-3"><li><p>(i)  Section 1.a.i</p><p>1.a.i<br />      </p></li></ul></li></ul></li></ul>""")
  }

  it should "be able to reference sections with custom symbols and formats" in {
    val text =
      """^ Section 1
        |^^ Section 1.a
        |^^^(s1ai(symbol:'Decimal';format:'Period')) Section 1.a.i
        |
        |[[s1ai]]
      """.stripMargin
    resultShouldBe(forReview(text, Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1<br /></p><ul class="list-lvl-2"><li><p>(a)  Section 1.a<br /></p><ul class="list-lvl-3"><li><p>1.  Section 1.a.i</p><p>1.a.1<br />      </p></li></ul></li></ul></li></ul>""")
  }

  it should "be able to override section symbols and formats" in {
    resultShouldBe(forReview("^ Section 1", Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'UpperRoman'; format: 'Period')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>I.  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'LowerLetter'; format: 'Parens')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>(a)  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'UpperLetter'; format: 'RightParen')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>A)  Section 1</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'Hide'; format: 'RightParen')) Section 1", Map()), """<ul class="list-lvl-1"><li><p>  Section 1</p></li></ul>""")
  }

  it should "be able to override subsequent section symbols and formats" in {
    resultShouldBe(forReview("^ Section 1^ Section 2", Map()), """<ul class="list-lvl-1"><li><p>1.  Section 1</p></li><li><p>2.  Section 2</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'LowerLetter'; format: 'Parens')) Section 1^ Section 2", Map()), """<ul class="list-lvl-1"><li><p>(a)  Section 1</p></li><li><p>(b)  Section 2</p></li></ul>""")
    resultShouldBe(forReview("^(_(symbol: 'LowerLetter')) Section 1^ Section 2^(_(format: 'Parens')) Section 3^ Section 4", Map()), """<ul class="list-lvl-1"><li><p>a.  Section 1</p></li><li><p>b.  Section 2</p></li><li><p>(c)  Section 3</p></li><li><p>(d)  Section 4</p></li></ul>""")
  }

  it should "not be able to emphasize sections" in {
    resultShouldBe(forReview("* ^ Section 1 *", Map()), """<p class="no-section">* </p><ul class="list-lvl-1"><li><p>1.  Section 1 *</p></li></ul>""")
    resultShouldBe(forReview("** ^ Section 1 **", Map()), """<p class="no-section">** </p><ul class="list-lvl-1"><li><p>1.  Section 1 **</p></li></ul>""")
    resultShouldBe(forReview("*** ^ Section 1 ***", Map()), """<p class="no-section">*** </p><ul class="list-lvl-1"><li><p>1.  Section 1 ***</p></li></ul>""")
  }

  it should "not be able to emphasize conditionals" in {
    resultShouldBe(forReview("* <%[[var1:Number]]%>{{var1 > 0 => test}} *", Map("var1" -> "1")), """<p class="no-section">* test *</p>""")
    resultShouldBe(forReview("** <%[[var1:Number]]%>{{var1 > 0 => test}} **", Map("var1" -> "1")), """<p class="no-section">** test **</p>""")
    resultShouldBe(forReview("*** <%[[var1:Number]]%>{{var1 > 0 => test}} ***", Map("var1" -> "1")), """<p class="no-section">*** test ***</p>""")
  }

  it should "not be able to emphasize across newlines" in {
    resultShouldBe(forReview("* This is \n text. *", Map()), """<p class="no-section">* This is <br /> text. *</p>""")
    resultShouldBe(forReview("** This is \n text. **", Map()), """<p class="no-section">** This is <br /> text. **</p>""")
    resultShouldBe(forReview("*** This is \n text. ***", Map()), """<p class="no-section">*** This is <br /> text. ***</p>""")
  }

  it should "parse unterminated emphasis as a normal star character" in {
    val text = "lorem * ipsum"
    resultShouldBe(forReview(text), """<p class="no-section">lorem * ipsum</p>""")
  }

  it should "parse eth address and render them properly" in {
    val text = "[[my address:EthAddress]]"
    resultShouldBe(forReview(text, Map("my address" -> "0x30c6738E9A5CC946D6ae1f176Dc69Fa1663b3b2C")), """<p class="no-section">30c6738e9a5cc946d6ae1f176dc69fa1663b3b2c</p>""")
  }

  it should "accept expressions for conditional blocks greater than" in {
    val text =
      """<%[[var1:Number]][[var2:Number]]%>{{var1 > var2 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin
    resultShouldBe(forReview(text, Map("var1" -> "112", "var2" -> "16")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "accept expressions for conditional blocks greater or equal" in {
    val text =
      """<%[[var1:Number]][[var2:Number]]%>{{var1 >= var2 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin
    resultShouldBe(forReview(text, Map("var1" -> "112", "var2" -> "16")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
    resultShouldBe(forReview(text, Map("var1" -> "16", "var2" -> "16")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
    resultShouldBe(forReview(text, Map("var1" -> "15", "var2" -> "16")), "")
  }

  it should "accept expressions with just a variable" in {
    val text =
      """{{var1 "this is a variable" => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin
    resultShouldBe(forReview(text, Map("var1" -> "true")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "accept expressions for conditional blocks lesser than" in {
    val text =
      """[[#var1:Number]][[#var2:Number]]{{var1 < var2 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var1" -> "12", "var2" -> "16")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "accept nested expressions for conditional" in {
    val text =
      """<%[[var1:Number]] [[var2:Number]]%>{{(var1 < var2) && (var1 < var2) => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var1" -> "12", "var2" -> "116")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "be able to define a default value" in {
    val clauseText = "This is my clause. [[contractor:Text(\"Hello my friend\")]]"

    structureAgreement(clauseText).toOption.flatMap(_.executionResult.getVariable("contractor").flatMap(_.defaultValue)) match {
      case Some(OneValueParameter(StringConstant(str,_))) => str shouldBe "Hello my friend"
      case result => fail(result.toString)
    }
  }

  it should "be able to define multiple default values" in {
    val clauseText = "This is my clause. [[contractor:Choice(\"First option\", \"Second option\")]]"

    structureAgreement(clauseText).toOption.flatMap(_.executionResult.getVariable("contractor").flatMap(_.defaultValue)) match {
      case Some(ListParameter(vector)) =>
        vector(0).asInstanceOf[StringConstant].value shouldBe "First option"
        vector(1).asInstanceOf[StringConstant].value shouldBe "Second option"
      case result => fail(result.toString)
    }
  }

  it should "not be able to define a default value for a number that is not a number" in {
    val clauseText = "This is my clause. [[contractor:Number(\"Hello my friend\")]]"

    structureAgreement(clauseText) match {
      case Left(ex) => ex shouldBe "the constructor type should be Number but is Text"
      case Right(_) => fail("should fail")
    }
  }

  it should "be able to define a default value for a number " in {
    val clauseText = "This is my clause. [[contractor:Number(24)]]"

    structureAgreement(clauseText) match {
      case Right(t) => t.executionResult.getVariable("contractor") match {
        case Some(variable) => variable.defaultValue match {
          case Some(OneValueParameter(NumberConstant(n,_))) => n shouldBe BigDecimal("24")
          case something => fail("default value is not correct:" + something)
        }
        case None => fail("variable not found")
      }
      case Left(ex) => fail(ex)
    }
  }

  it should "be able to define a default value for a date by parsing the date" in {
    val clauseText = "This is my clause. [[contractor:Date(\"2017-06-24\")]]"
    structureAgreement(clauseText) match {
      case Right(t) => t.executionResult.getVariable("contractor") match {
        case Some(variable) => variable.defaultValue match {
          case Some(OneValueParameter(StringConstant(text,_))) => text shouldBe "2017-06-24"
          case something => fail("default value is not correct:" + something)
        }
        case None => fail("contractor variable not found")
      }
      case Left(ex) => fail(ex)
    }
  }

  it should "be able to define a default value for a date time by parsing the date" in {
    val clauseText = "This is my clause. [[contractor:DateTime(\"2017-06-24 13:45:00\")]]"

    structureAgreement(clauseText) match {
      case Right(t) => t.executionResult.getVariable("contractor").flatMap(_.defaultValue) match {
        case Some(OneValueParameter(StringConstant(text,_))) => text shouldBe "2017-06-24 13:45:00"
        case something => fail("default value is not correct:" + something)
      }
      case Left(ex) => fail(ex)
    }
  }

  it should "boolean with composable" in {
    val text =
      """<%[[var1:YesNo]][[var2:YesNo]]%>{{var1 && var2 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var1" -> "true", "var2" -> "true")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "be able to use constants in comparaison expressions" in {
    val text =
      """[[#var1:Number]]{{var1 > 10 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var1" -> "12")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "be able to use constants in comparaison expressions with equals too" in {
    val text =
      """[[#var1:Number]]{{var1 = 12 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var1" -> "12")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "be able to use constants in equals expressions" in {
    val text =
      """[[#var1:Number]]{{var1 = 10 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var1" -> "10")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }
  it should "be able to use aliasing " in {
    val text =
      """[[#var2:Number]][[@var1 = var2 + 10]]{{var1 > 10 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var2" -> "10")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "be able to use aliasing 2" in {
    val text =
      """[[#var2:Number]][[@var1 = 10 + var2]]{{var1 > 10 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var2" -> "10")), """<p class="no-section">iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "be able to use aliasing 3" in {
    val text = """[[#var2:Number]][[#var1:Number]][[@var3 = var1 + var2]][[var3]]{{var3 > 39 =>iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    resultShouldBe(forReview(text, Map("var2" -> "10", "var1" -> "30")), """<p class="no-section">40iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj</p>""")
  }

  it should "handle cyclic dependencies" in {
    val text =
      """[[#var1:Number]][[@var2 = var1 + var3]][[@var3 = var1 + var2]][[var1]][[var2]][[var3]]{{var3 > 39 => iojiwofjiowejf iwjfiowejfiowejfiowejfiowefj}}""".stripMargin

    structureAgreement(text) match {
      case Left(ex) =>
        ex shouldEqual "alias expression uses undefined variables var3"
      case Right(_) =>
        fail("this should fail")
    }
  }

  it should "handle cyclic dependencies with self dependency" in {
    val text =
      """[[#var1:Number]][[@var2 = var1 + var2]][[var1]][[var2]]""".stripMargin

    structureAgreement(text) match {
      case Left(ex) =>
        ex shouldEqual "alias expression uses undefined variables var2"
      case Right(_) =>
        fail("this should fail")
    }
  }

  it should "not see an alias as a forward reference " in {
    val text =
      """[[@var1 = 10]][[@var2 = var1 + 10]][[var1]][[var2]]""".stripMargin

    forReview(text) shouldBe Right("""<p class="no-section">1020</p>""")
  }

  it should "handle cyclic dependencies with only aliases" in {
    val text =
      """
        |[[@a = b + 10]]
        |[[@b = c + 10]]
        |[[@c = a + 10]]
      """.stripMargin

    structureAgreement(text) match {
      case Left(ex) =>
        ex shouldEqual "alias expression uses undefined variables b"
      case Right(res) =>
        fail(s"this should fail")
    }
  }

  it should "handle cyclic dependencies in constructor" in {
    val text =
      """[[var1:Number]][[var2:Number(var1 + var3)]][[var3:Number(var1 + var2)]]""".stripMargin

    structureAgreement(text) match {
      case Left(ex) =>
        ex shouldEqual "error while processing the new variable var2. The variables var3 are used in the constructor but have not been defined"
      case Right(_) =>
        fail(s"this should fail")
    }
  }

  it should "handle code blocks" in {
    val text=
      """<%
        |# this is a comment
        |[[Var1:Number]]
        |[[@Var2 = Var1 + 4]]
        |[[My Contract Address:EthAddress]]
        |[[My Interface:SmartContractMetadata]]
        |[[My Contract:EthereumCall(contract:My Contract Address;
        |interface:My Interface;
        |network:"4";
        |function:"function"
        |)]]
        |%>[[Var2]]""".stripMargin

    resultShouldBe(forReview(text, Map("Var1" -> "2202")), """<p class="no-section">2,206</p>""")
  }

  it should "handle not logic" in {
    val text="""<%[[My Var:YesNo]][[Another:YesNo]]%>{{My Var && !Another => iojiowejfiowejfiowejfioewjf }}""".stripMargin

    structureAgreement(text) match {
      case Right(t) => t.executionResult.variables.map(_.name.name).toSet shouldBe Set("My Var", "Another")
      case Left(ex) => fail(ex)
    }
  }

  it should "let you put different quote characters in a string" in {
    val text="""{{My Var "that's it!" => iojiowejfiowejfiowejfioewjf }}""".stripMargin

    structureAgreement(text) match {
      case Right(compiledDocument) =>
        compiledDocument.executionResult.variables.map(_.name.name).toSet shouldBe Set("My Var")
        compiledDocument.executionResult.variables.map(_.description).head shouldBe Some("that's it!")
      case Left(ex) => fail(ex)
    }
  }

  it should "throw an error if the variable types used in an alias don't match" in {
    val text=
      """
        [[Var 1:Number]]
        [[Var 2:Text]]
        [[@Bad Var = Var 1 + Var 2]]
        |""".stripMargin

    structureAgreement(text) match {
      case Left(ex) =>
        ex shouldBe "left and right expression are of incompatible types.Number & Text in Var 1 & Var 2"
      case Right(_) =>
        fail("should fail")
    }
  }

  it should "let use calculation between periods and dates" in {
    val text=
      """<%[[Var 1:Period]]
        [[Var 2:DateTime]]
        [[@New Date = Var 1 + Var 2]]%>[[New Date]]""".stripMargin

    resultShouldBe(forReview(text,Map("Var 1" -> "1 day","Var 2" -> (LocalDateTime.now
      .withYear(2018)
      .withMonth(1)
      .withDayOfMonth(1)
      .withHour(10)
      .withMinute(10)
      .withSecond(0)
      .toEpochSecond(ZoneOffset.UTC) * 1000).toString
    )),"""<p class="no-section">January 2, 2018 10:10:00</p>""")
  }

  it should "let use calculation between periods and dates with constants" in {
    val text=
      """<%[[Var 2:DateTime]]
        [[@New Date = Var 2 + "1 day"]]%>[[New Date]]""".stripMargin

    resultShouldBe(forReview(text,Map("Var 1" -> "1 day","Var 2" -> (LocalDateTime.now
      .withYear(2018)
      .withMonth(1)
      .withDayOfMonth(1)
      .withHour(10)
      .withMinute(10)
      .withSecond(0)
      .toEpochSecond(ZoneOffset.UTC) * 1000).toString
    )), """<p class="no-section">January 2, 2018 10:10:00</p>""")
  }

  it should "handle a set of conditionals " in {
    val text=
      """{{
        |{{Condition 1 "This is a condition" => Condition 1}}
        |{{Condition 2 "This is another condition" => Condition 2}}
        |}}""".stripMargin

    resultShouldBe(forReview(text, Map(
      "Condition 1" -> "false",
      "Condition 2" -> "false"
    )),"")

    resultShouldBe(forReview(text, Map(
      "Condition 1" -> "true",
      "Condition 2" -> "false"
    )), """<p class="no-section">Condition 1</p>""")

    resultShouldBe(forReview(text, Map(
      "Condition 1" -> "false",
      "Condition 2" -> "true"
    )), """<p class="no-section">Condition 2</p>""")

    resultShouldBe(forReview(text, Map(
      "Condition 1" -> "true",
      "Condition 2" -> "true"
    )), """<p class="no-section">Condition 1</p>""")
  }

  it should "clean the text if there is too many returns" in {
    val text=
      """this is a first line
        |
        |this should not be changed
        |
        |
        |
        |but here yes
        |
        |
        |here too""".stripMargin
    resultShouldBe(forReview(text),
          """<p class="no-section">this is a first line</p><p class="no-section">this should not be changed<br /><br /></p><p class="no-section">but here yes<br /></p><p class="no-section">here too</p>""".stripMargin)
  }

  it should "be able to break pages" in {
    val text =
      """first paragraph of text
      |\pagebreak
      |second paragraph of text""".stripMargin
    resultShouldBe(forReview(text),
      """<p class="no-section">first paragraph of text<br /></p><p class="no-section"><hr /></p><p class="no-section">second paragraph of text</p>""")
  }

  it should "be able to align lines centered" in {
    val text =
      """first paragraph of text
      |
      |\centeredsecond paragraph of text
      |
      |third paragraph of text""".stripMargin
    resultShouldBe(forReview(text),
      """<p class="no-section">first paragraph of text</p><p class="no-section align-center">second paragraph of text</p><p class="no-section">third paragraph of text</p>""")
  }

  it should "be able to align lines to the right" in {
    val text =
      """first paragraph of text
      |
      |\rightsecond paragraph of text
      |
      |third paragraph of text""".stripMargin
    resultShouldBe(forReview(text),
      """<p class="no-section">first paragraph of text</p><p class="no-section align-right">second paragraph of text</p><p class="no-section">third paragraph of text</p>""")
  }

  it should "be able to align lines to the right with three-quarters spacing" in {
    val text =
      """first paragraph of text
      |
      |\right-three-quarterssecond paragraph of text
      |
      |third paragraph of text""".stripMargin
    resultShouldBe(forReview(text),
      """<p class="no-section">first paragraph of text</p><p class="no-section align-right-three-quarters">second paragraph of text</p><p class="no-section">third paragraph of text</p>""")
  }

  it should "clean the text if there is invisible variables in the middle" in {
    val text=
      """this is a first line
        |
        |this should not be changed
        |[[#Test 1]]
        |[[#Test 2]]
        |
        |but here yes
        |
        |
        |here too""".stripMargin
    resultShouldBe(forReview(text),
      """<p class="no-section">this is a first line</p><p class="no-section">this should not be changed<br /><br /></p><p class="no-section">but here yes<br /></p><p class="no-section">here too</p>""".stripMargin)
  }

  it should "clean the text if there is a space before a dot" in {
    val text=
      """this is a first line .
        |this is another line ...
        |even with multiple spaces      .""".stripMargin

    resultShouldBe(forReview(text),
        """<p class="no-section">this is a first line.<br />this is another line ...<br />even with multiple spaces.</p>""".stripMargin)
  }

 // this is the tricky handling for conditionals, make sure that this works properly
  it should "handle properly conditional blocks highlights with sections" in {
    val text="""{{Try "try this logic" => ^ This is a test}}""".stripMargin

    resultShouldBe(forPreview(text, Map("Try" -> "true")),
    """<ul class="list-lvl-1"><li><div class="openlaw-paragraph paragraph-1"><p>1. <span class="markdown-conditional-block"> This is a test</span></p></div></li></ul>""")
  }

  it should "handle decimals in constants as well" in {
    val text =
      """
        |[[#Variable1:Number(0.34)]]
        |[[@My Alias = Variable1 + 0.56]]
        |[[My Alias | noTrailingZeros]]""".stripMargin.replaceAll("\n","")

    resultShouldBe(forReview(text, Map("Variable1" -> "0.34")), """<p class="no-section">0.9</p>""")
  }

  it should "handle decimals in divisions" in {
    val text =
      """
        |[[#Variable1:Number]]
        |[[@My Alias = (Variable1 / 100) * 4 ]]
        |[[My Alias | noTrailingZeros]]""".stripMargin.replaceAll("\n","")

    resultShouldBe(forReview(text, Map("Variable1" -> "34")), """<p class="no-section">1.36</p>""")
  }

  it should "be able to organize sections for variables" in {
    val text =
      """
        |==My first section==
        |[[Variable 1:Number]]
        [[Variable 2:Number]]
        |
        |==My second section==
        |[[Variable 3:Number]]
        [[Variable 4:Number]]
        |<%
        |==My second section==
        |[[Variable 5:Number]]
        [[Variable 6:Number]]
        |%>
      """.stripMargin

    structureAgreement(text) match {
      case Right(agreement) =>
        val sections = agreement.executionResult.sections
        sections("My first section").map(_.name) should contain theSameElementsAs Seq("Variable 1", "Variable 2")
        sections("My second section").map(_.name) should contain theSameElementsAs Seq("Variable 3", "Variable 4", "Variable 5", "Variable 6")

        agreement.executionResult.variables.map(_.name.name).toSet should contain theSameElementsAs Set(
          "Variable 1",
          "Variable 2",
          "Variable 3",
          "Variable 4",
          "Variable 5",
          "Variable 6"
        )
      case Left(ex) => fail(ex)
    }
  }

  it should "not break a paragraph from a variable" in {
    val source = """Effective Date:  [[Effective Date: DateTime]]
                   |
                   |In consideration and as a condition of my employment, continued employment, or payment for services rendered as an independent contractor, consultant, or advisor ("Service Relationship") by [[Company]] {{Corporation "Is the company a corporation?" => , a [[StateOfIncorporation "What state is the first party incorporated in?"]] corporation, }} {{LLC "An LLC?" => a [[StateOfIncorporation "What state company incorporated in?"]] limited liability company, }} {{PBC "A Public Benefit Corporation?" => , a [[StateOfIncorporation "What state is the company incorporated in?"]] public benefit corporation, }}  or any of its current or future subsidiaries, affiliates, successors or assigns (collectively, the "Company"), and my receipt of compensation now and hereafter paid to me by the Company, I hereby agree as follows:
                   |
                   |^**Confidential Information Protections.**
                   |
                   |^^	**Confidential Information.**  I agree that all information, whether or not in writing, concerning the Company’s business, technology, business relationships or financial affairs which the Company has not released to the general public (collectively, "Confidential Information") is and will be the exclusive property of the Company. By way of illustration, Confidential Information may include information or material which has not been made generally available to the public, such as: (i) corporate information, including plans, strategies, methods, policies, resolutions, negotiations, or litigation; (ii) marketing information, including strategies, methods, customer identities or other information about customers, prospect identities or other information about prospects, or market analyses or projections; (iii) financial information, including cost and performance data, debt arrangements, equity structure, investors and holdings, purchasing and sales data and price lists; and (d) operational and technological information, including plans, specifications, manuals, forms, templates, software, designs, methods, procedures, formulas, discoveries, inventions, improvements, concepts and ideas; and (e) personnel information, including personnel lists, reporting or organizational structure, resumes, personnel data, compensation structure, performance evaluations and termination arrangements or documents. Confidential Information also includes information received in confidence by the Company from its customers or suppliers or other third parties.
                   |
                   |^^ **Nondisclosure; Recognition of Company’s Rights.**  I will not, at any time, without the Company’s prior written permission, either during or after my Service Relationship with the Company, disclose any Confidential Information to anyone outside of the Company, or use or permit to be used any Confidential Information for any purpose other than the performance of my duties as a Service Provider of the Company. I will cooperate with the Company and use my best efforts to prevent the unauthorized disclosure of all Proprietary Information. I will deliver to the Company all copies of Confidential Information in my possession or control upon the earlier of a request by the Company or termination of my service relationship with the Company.""".stripMargin

    (forReview(source, Map(),ParagraphEdits(Map(2 -> "hello world"))),
    """<p class="no-section">Effective Date:  [[Effective Date]]</p><p class="no-section">In consideration and as a condition of my employment, continued employment, or payment for services rendered as an independent contractor, consultant, or advisor ("Service Relationship") by [[Company]]     or any of its current or future subsidiaries, affiliates, successors or assigns (collectively, the "Company"), and my receipt of compensation now and hereafter paid to me by the Company, I hereby agree as follows:</p><ul class="list-lvl-1"><li><p>1. hello world</p><ul class="list-lvl-2"><li><p>(a) 	<strong>Confidential Information.</strong>  I agree that all information, whether or not in writing, concerning the Company’s business, technology, business relationships or financial affairs which the Company has not released to the general public (collectively, "Confidential Information") is and will be the exclusive property of the Company. By way of illustration, Confidential Information may include information or material which has not been made generally available to the public, such as: (i) corporate information, including plans, strategies, methods, policies, resolutions, negotiations, or litigation; (ii) marketing information, including strategies, methods, customer identities or other information about customers, prospect identities or other information about prospects, or market analyses or projections; (iii) financial information, including cost and performance data, debt arrangements, equity structure, investors and holdings, purchasing and sales data and price lists; and (d) operational and technological information, including plans, specifications, manuals, forms, templates, software, designs, methods, procedures, formulas, discoveries, inventions, improvements, concepts and ideas; and (e) personnel information, including personnel lists, reporting or organizational structure, resumes, personnel data, compensation structure, performance evaluations and termination arrangements or documents. Confidential Information also includes information received in confidence by the Company from its customers or suppliers or other third parties.</p></li><li><p>(b)  <strong>Nondisclosure; Recognition of Company’s Rights.</strong>  I will not, at any time, without the Company’s prior written permission, either during or after my Service Relationship with the Company, disclose any Confidential Information to anyone outside of the Company, or use or permit to be used any Confidential Information for any purpose other than the performance of my duties as a Service Provider of the Company. I will cooperate with the Company and use my best efforts to prevent the unauthorized disclosure of all Proprietary Information. I will deliver to the Company all copies of Confidential Information in my possession or control upon the earlier of a request by the Company or termination of my service relationship with the Company.</p></li></ul></li></ul>""")
  }

  it should "compile this piece" in {
    val text = """"==Effective Date==
              [[Effective Date: Date | date]]

                 ==Company Information -
                 What's the basic information for company?==
                 [[Company Name]]
                 [[Company Street]]
                 [[Company City]]
                 [[Company State]]
                 [[Company Zip]]

                 ==Company Signatory
                 Who will be the company signatory?==
                 [[First Name of Company Signatory]]
                 [[Last Name of Company Signatory]]

                 ==Employee Information
                 Enter some basic employee information below==
                 [[Employee First Name]]
                 [[Employee Last Name]]
                 [[Employee Street]]
                 [[Employee City]]
                 [[Employee State]]
                 [[Employee Zip]]
                 [[Additional Agreements]]
                 [[Additional Employee information]]


                 [[Employee Offer Letter: Template("Employee Offer Letter")]]

                 {{Additional Employee information "Do you need additional information?" =>

                 ==Employee Information
                 Enter some basic employee information below==
                 [[Employee Information Plus 1:Text]]
                 }}

                 {{Additional Agreements "Will the employee be signing additional agreements?" => {{Confidentiality Agreement "A confidentiality agreement?" => [[CIAA: Template("Confidential Information and Invention Assignment Agreement")]]}} {{Dispute Resolution "An Alternative Dispute Resolution Agreement?" => [[ADR: Template("Alternative Dispute Resolution Agreement")]]}} }}""""

    compiledTemplate(text)
  }

  it should "work with employee offer letter" in {
    val text  =
      """
        |**[[Company Name: Text | Uppercase]]**
        |[[Company Street]]
        |[[Company City]], [[Company State]] [[Company Zip]]
        |
        |[[Effective Date: Date | date]]
        |
        |[[Employee First Name]] [[Employee Last Name]]
        |[[Employee Street]]
        |[[Employee City]], [[Employee State]] [[Employee Zip]]
        |
        |**Re:  Offer Letter**
        |
        |Dear [[Employee First Name]]:
        |
        |We're excited to offer you a position with""".stripMargin

    resultShouldBe(forReview(text,Map()),
      """<p class="no-section"><br /><strong>[[Company Name]]</strong><br />[[Company Street]]<br />[[Company City]], [[Company State]] [[Company Zip]]</p><p class="no-section">[[Effective Date]]</p><p class="no-section">[[Employee First Name]] [[Employee Last Name]]<br />[[Employee Street]]<br />[[Employee City]], [[Employee State]] [[Employee Zip]]</p><p class="no-section"><strong>Re:  Offer Letter</strong></p><p class="no-section">Dear [[Employee First Name]]:</p><p class="no-section">We're excited to offer you a position with</p>""".stripMargin)
  }


  it should "be able to override paragraphs" in {
    val text =
      """hello my friend
        |
        |^ this is a new paragraph [[My Variable]]
        |
        |and this is too
      """.stripMargin

    resultShouldBe(forReview(text = text, paragraphs = ParagraphEdits(Map(2 -> "now I want this new text [[My Variable]]"))), """<p class="no-section">hello my friend</p><ul class="list-lvl-1"><li><p>1.  this is a new paragraph [[My Variable]]</p><p>now I want this new text [[My Variable]]</p></li></ul>""")
  }

  it should "not eat some of the content" in {
    val text =
      """
        |[[ConsenSys AG Signatory Email: Identity]]
        |
        |_______________________
        |Joseph Lubin, Mitglied des Verwaltungsrates (Board Member) ConsenSys AG
        |
        |On behalf of the Contractor:
        |
        |[[Contractor Email:  Identity]]
        |_______________________
        |By: [[Contractor First Name]] [[Contractor Last Name]]
      """.stripMargin

    resultShouldBe(forReview(text = text, params = Map()), """<p class="no-section"><br /></p><p class="no-section">_______________________<br />Joseph Lubin, Mitglied des Verwaltungsrates (Board Member) ConsenSys AG</p><p class="no-section">On behalf of the Contractor:</p><p class="no-section"><br />_______________________<br />By: [[Contractor First Name]] [[Contractor Last Name]]<br />      </p>""")
  }

  it should "give you the list of used variables in the template" in {
    val text =
      """<%
        |[[var1:Number]]
        |[[var2:Text]]
        |%>
        |
        |[[var1]]
        |""".stripMargin

    structureAgreement(text, Map()) match {
      case Right(agreement) =>
        agreement.executionResult.executedVariables.map(_.name) shouldBe Seq("var1")
      case Left(ex) => fail(ex)
    }

  }

  it should "give you the list of used variables in the template even if it is used in a constructor" in {
    val text =
      """<%
        |[[var2:Number]]
        |[[var1:Number(var2)]]
        |%>
        |
        |[[var1]]
        |""".stripMargin

    structureAgreement(text, Map()) match {
      case Right(agreement) =>
        agreement.executionResult.getExecutedVariables.map(_.name) shouldBe Seq("var1", "var2")
      case Left(ex) => fail(ex)
    }
  }

  it should "give you the list of used variables in the template even if it is used in an alias" in {
    val text =
      """<%
        |[[var2:Number]]
        |[[@var3 = var2 + 10]]
        |[[var1:Number(var3)]]
        |%>
        |
        |[[var1]]
        |""".stripMargin

    structureAgreement(text, Map()) match {
      case Right(agreement) =>
        agreement.executionResult.getExecutedVariables.map(_.name) shouldBe Seq("var1", "var2")
      case Left(ex) => fail(ex)
    }
  }

  it should "not seen the right part of an expression as executed if it should not with 'and'" in {
    val text =
      """<%
        |[[var1:Number]]
        |[[var2:Number]]
        |%>
        |{{(var1 > 10) && (var2 > 20) => iuhiuhuih}}
        |""".stripMargin

      structureAgreement(text, Map("var1" -> "5" , "var2" -> "40")) match {
        case Right(agreement) =>
          agreement.executionResult.getExecutedVariables.map(_.name) shouldBe Seq("var1")
        case Left(ex) => fail(ex)
      }
  }

  it should "not seen the right part of an expression as executed if it should not with 'or'" in {
    val text =
      """<%
        |[[var1:Number]]
        |[[var2:Number]]
        |%>
        |{{(var1 > 10) || (var2 > 20) => hihiuhuih }}
        |""".stripMargin


    structureAgreement(text, Map("var1" -> "25" , "var2" -> "40")) match {
      case Right(agreement) =>
        agreement.executionResult.executedVariables.map(_.name) shouldBe Seq("var1")
      case Left(ex) => fail(ex)
    }
  }

  it should "handle the old conditional syntax" in {
    val text =
      """<%
        |[[var1:Number]]
        |[[var2:Number]]
        |%>
        |{{((var1 > 10) || (var2 > 20)) hihiuhuih }}
        |""".stripMargin


    structureAgreement(text, Map("var1" -> "25" , "var2" -> "40")) match {
      case Right(agreement) =>
        agreement.executionResult.executedVariables.map(_.name) shouldBe Seq("var1")
      case Left(ex) => fail(ex)
    }
  }

  it should "be able to list a variable that is in bold" in {
    val text = "** [[My Var:Text]] **"

    structureAgreement(text, Map()) match {
      case Right(document) =>
        document.executionResult.variables.map(_.name.name) should contain("My Var")
      case Left(ex) => fail(ex)
    }
  }

  it should "take the default value expression if none has been specified" in {
    val text = """[[My Number:Number(12)]][[My Number 2:Number(My Number + 10)]]""".stripMargin

    resultShouldBe(forReview(text, Map()), """<p class="no-section">1222</p>""")
  }

  it should "read a property from an address" in {
    val text ="<%[[My Address:Address]]%>[[My Address.country]]"

    resultShouldBe(forReview(text, Map("My Address" -> AddressType.internalFormat(Address(
      city = "a certain city",
      state = "a state",
      country = "United States",
      zipCode = "102030392",
      formattedAddress = "some kind of formatted address"
    )))), """<p class="no-section">United States</p>""")
  }

  it should "validate and make sure you do not use an invalid property" in {
    val text ="<%[[My Address:Address]]%>[[My Address.badProperty]]"

    structureAgreement(text) match {
      case Right(_) => fail("should fail")
      case Left(msg) => msg shouldBe "property 'badProperty' not found for type Address"
    }
  }

  it should "round number" in {
    val text ="<%[[My Number:Number]]%>[[My Number | rounding(2)]]"

    resultShouldBe(forReview(text, Map("My Number" -> "0.33333333333")),"""<p class="no-section">0.33</p>""")
  }

  it should "format number" in {
    val text ="<%[[My Number:Number]]%>[[My Number]]"

    resultShouldBe(forReview(text, Map("My Number" -> "1000000000")),"""<p class="no-section">1,000,000,000</p>""")
  }

  it should "round number with expression" in {
    val text ="<%[[My Number:Number]] [[Rounding Number:Number]]%>[[My Number | rounding(Rounding Number)]]"

    resultShouldBe(forReview(text, Map("My Number" -> "0.333333333333333", "Rounding Number" -> "2")), """<p class="no-section">0.33</p>""")
  }

  it should "handle validation" in {
    val text =
      """<%
         [[My Number:Number]]
         %>

         [[number validation:Validation(
         condition: My Number > 5;
         errorMessage:"My Number needs to be higher than 5"
         )]]
      """.stripMargin

    structureAgreement(text, Map("My Number" -> "3")) match {
      case Right(structuredAgreement) =>
        structuredAgreement.executionResult.validate() should contain("My Number needs to be higher than 5")
      case Left(ex) => fail(ex)
    }
  }

  it should "verify that conditionals are of the correct type" in {
    val text =
      """
         [[number:Number]]
         {{ number + 401 => should not work}}
      """.stripMargin

    structureAgreement(text) match {
      case Right(_) =>
        fail("should fail")
      case Left(ex) => ex shouldBe "Conditional expression number+401 is of type NumberType instead of YesNo"
    }
  }

  it should "verify that conditionals work with choices" in {
    val text =
      """<%
         [[City:Choice("Zurich", "New York")]]
         [[my city:City]]
         %>{{ my city = "Zurich" => hello world}}""".stripMargin

    resultShouldBe(forReview(text, Map("my city" -> "Zurich")), """<p class="no-section">hello world</p>""")
  }

  it should "throw an exception if we have an unknown type" in {
    val text =
      """<%
         [[City:My City]]
         %>{{ City = "Zurich" => hello world}}""".stripMargin

    structureAgreement(text) match {
      case Right(_) =>
        fail("should fail")
      case Left(ex) =>
        ex shouldBe "error while processing the new variable City. The variable has type My City but it does not exist"
    }
  }

  it should "allow a value from the specified choices" in {
    val text =
      """
         [[Options:Choice("one", "two", "three")]]
         [[option:Options]]
      """.stripMargin


    structureAgreement(text, Map("option" -> "two")) match {
      case Right(agreement) =>
        agreement.executionResult.getVariableValue[String](VariableName("option")) shouldBe Some("two")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "disallow a value not from the specified choices" in {
    val text =
      """
         [[Options:Choice("one", "two", "three")]]
         [[option:Options]]
      """.stripMargin

    val result = structureAgreement(text, Map("option" -> "four"))
    result shouldBe Left("the value four is not part of the type Options")
  }

  it should "allow specifying values from a structure" in {
    val text =
      """
         [[Name:Structure(
         first: Text;
         last: Text
         )]]
         [[name1:Name]]
      """.stripMargin

    structureAgreement(text) match {
      case Right(agreement) =>
        val structureType = agreement.executionResult.findVariableType(VariableTypeDefinition("Name")).getOrElse(NumberType)
        structureType === NumberType shouldBe false
        val newAgreement = structureAgreement(text, Map("name1" -> structureType.internalFormat(Map(VariableName("first") -> "John", VariableName("last") -> "Doe")))).toSeq.head

        service.parseExpression("name1.first").map(_.evaluate(newAgreement.executionResult)) shouldBe Right(Some("John"))
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "use property as an expression in an alias" in {
    val text =
      """
         [[address:Address]]
         [[@My Country = address.country]]
         [[@My Id = address.placeId]]
      """.stripMargin

    structureAgreement(text, Map("address" -> AddressType.internalFormat(Address(
      placeId = "placeId",
      streetName = "streetName",
      streetNumber = "streetNumber",
      city = "city",
      state = "state ",
      country = "Country",
      zipCode = "zipCode",
      formattedAddress = "formattedAddress"
    )))) match {
      case Right(structured) =>
        val result = structured.executionResult.getAlias("My Id").flatMap(_.evaluate(structured.executionResult))

        result shouldBe Some("placeId")
      case Left(ex) => fail(ex)
    }
  }

  it should "redefine the variable from Text to YesNo " in {
    val text =
      """
         [[My Conditional]]
         {{My Conditional "this is a question" => hello }}
      """.stripMargin

    structureAgreement(text, Map("My Conditional" -> "true")) match {
      case Right(structured) =>
        val variableDefinition = structured.executionResult.variables.head

        variableDefinition.name.name shouldBe "My Conditional"
        variableDefinition.variableTypeDefinition.map(_.name) shouldBe Some(YesNoType.name)
        variableDefinition.description shouldBe Some("this is a question")
      case Left(ex) => fail(ex)
    }
  }

  it should "redefine the variable from Text to YesNo even if it is nested" in {
    val text =
      """
         <%
         ==Test==
         [[My Conditional:YesNo "this is a question"]]
         [[BV]]
         %>
         {{My Conditional && BV =>
         {{My Conditional => hello }}
         |{{BV "this is a question" => world }}
         }}
      """.stripMargin

    structureAgreement(text, Map("BV" -> "true", "My Conditional" -> "true")) match {
      case Right(structured) =>
        val variableDefinition = structured.executionResult.variables.filter(_.name.name === "My Conditional").head

        variableDefinition.name.name shouldBe "My Conditional"
        variableDefinition.variableTypeDefinition.map(_.name) shouldBe Some(YesNoType.name)
        variableDefinition.description shouldBe Some("this is a question")
      case Left(ex) => fail(ex)
    }
  }

  it should "not see code block elements as executed" in {
    val text =
      """
        |<%
        |==Effective Date==
        |[[Effective Date: Date]]
        |
        |==Company Name and Address==
        |[[Company Name]]
        |[[Company Address:Address]]
        |[[Corporation:YesNo]]
        |[[LLC:YesNo]]
        |[[PBC:YesNo]]
        |[[State of Incorporation]]
        |
        |==Company Signatory==
        |[[Company Signatory First Name]]
        |[[Company Signatory Last Name]]
        |[[Company Signatory Position]]
        |
        |==Employee Information==
        |[[Employee First Name]]
        |[[Employee Last Name]]
        |[[Employee Address:Address]]
        |[[Recipient Address:EthAddress]]
        |
        |==Employee Position==
        |[[Employee Position]]
        |[[Employee Responsibilities]]
        |[[Position of Supervisor]]
        |
        |==Employee Documents==
        |[[Additional Agreements:YesNo]]
        |[[Confidentiality Agreement]]
        |[[Dispute Resolution]]
        |
        |==Restricted Stock Grant==
        |[[Stock Award:YesNo]]
        |[[Shares of Common Stock:Number]]
        |[[Grant Price]]
        |[[Board Action]]
        |
        |[[Governing Law]]
        |%>
        |
        |{{Additional Agreements "Will the employee be signing additional agreements?" =>
        |{{Confidentiality Agreement "A Confidentiality and Invention Assignment Agreement?" => }}
        |{{Dispute Resolution "An Alternative Dispute Resolution Agreement?" => }}
        |}}
        |
        |{{Corporation "Is the company a corporation?" => [[State of Incorporation]]}}
        |{{LLC "An LLC?" [[State of Incorporation]]}}
        |{{PBC "A Public Benefit Corporation?" [[State of Incorporation]]}}
        |
        |{{Stock Award "A Restricted Stock Grant?" => {{Board Action "Do you want to execute a unanimous action of the board?" => }}}}
      """.stripMargin

    structureAgreement(text, Map()) match {
      case Right(structured) =>
        structured.executionResult.executedVariables.map(_.name) shouldBe Seq("Additional Agreements" ,"Corporation", "LLC", "PBC", "Stock Award")

        structured.executionResult.getVariable("Dispute Resolution") match {
          case Some(variable) =>
            variable.variableTypeDefinition.map(_.name) shouldBe Some(YesNoType.name)
            variable.description shouldBe Some("An Alternative Dispute Resolution Agreement?")
          case None =>
            fail("Dispute Resolution not found!")
        }
      case Left(ex) => fail(ex)
    }
  }

  it should "see sections as non executed" in {
    val text =
      """
        |<%
        |==Conditional with Variables==
        |[[Conditional]]
        |[[Variable A]]
        |[[Variable B]]
        |%>
        |{{Conditional "Do you want to see the variables?" => The value of Variable A is [[Variable A]]. The value of Variable B is [[Variable B]].}}
      """.stripMargin

    structureAgreement(text) match {
      case Right(document) =>
        document.executionResult.executedVariables.map(_.name) shouldBe Seq("Conditional")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "parse [ and ] as characters if it is not the right variable format" in {
    val text = "[this is some text]"

    resultShouldBe(forReview(text), """<p class="no-section">[this is some text]</p>""")
  }

  it should "be able to define a header in the template" in {
    val text =
      """#########
         show title:false;
         template:test me;
         some value:here;
         description: multi
lines value
here;
         ###########################
         hello world""".stripMargin

    val Right(template) = compiledAgreement(text)

    resultShouldBe(forReview(text), """<p class="no-section">hello world</p>""")

    val actualHeader = template.header

    actualHeader.values("template") shouldBe "test me"
    actualHeader.values("some value") shouldBe "here"
    actualHeader.values("show title") shouldBe "false"
    actualHeader.values("description") shouldBe
      """multi
lines value
here""".stripMargin

    actualHeader.values.size shouldBe 4

    actualHeader.shouldShowTitle shouldBe false
  }

  it should "be able to show the title" in {
    val text =
      """#########
         show title:true;
         ###########################
         """.stripMargin

    val Right(template) = compiledAgreement(text)

    val actualHeader = template.header
    actualHeader.values("show title") shouldBe "true"
    actualHeader.values.size shouldBe 1

    actualHeader.shouldShowTitle shouldBe true
  }
}
