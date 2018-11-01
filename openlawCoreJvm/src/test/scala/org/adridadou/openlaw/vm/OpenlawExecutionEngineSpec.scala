package org.adridadou.openlaw.vm

import java.time.Clock

import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.{TemplateParameters, TemplateTitle}
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json

class OpenlawExecutionEngineSpec extends FlatSpec with Matchers {

  val parser = new OpenlawTemplateLanguageParserService(Clock.systemDefaultZone())
  val engine = new OpenlawExecutionEngine()

  "Openlaw engine" should "run a simple template" in {
    val text =
      """
        |<%
        |[[My Variable:Text]]
        |[[Other one:Number]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
      """.stripMargin

    val compiledTemplate = compile(text)
    val parameters = TemplateParameters("My Variable 2" -> "hello", "Other one" -> "334")
    engine.execute(compiledTemplate, parameters, Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        result.variables.map(_.name.name) shouldBe Seq("My Variable", "Other one")
      case Left(ex) => fail(ex)
    }
  }

  it should "wait for a template and then continue and run it" in {
    val text =
      """
        |<%
        |[[My Variable:Text]]
        |[[_:Text]]
        |[[_:Text]]
        |[[Other one:Number]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
        |
        |[[_:Template("Another Template")]]
      """.stripMargin

    val text2 = "it is just another template [[My Variable 2:Text]]"
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters = TemplateParameters("My Variable 2" -> "hello", "Other one" -> "334")
    engine.execute(compiledTemplate, parameters, Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionWaitForTemplate(VariableName("@@anonymous_3@@"), TemplateSourceIdentifier(TemplateTitle("Another Template")))
        result.variables.map(_.name.name) shouldBe Seq("My Variable","@@anonymous_1@@", "@@anonymous_2@@", "Other one", "@@anonymous_3@@")

        engine.resumeExecution(result, Map(TemplateSourceIdentifier(TemplateTitle("Another Template")) -> otherCompiledTemplate)) match {
          case Right(newResult) =>
            newResult.state shouldBe ExecutionFinished
            newResult.parentExecution.isDefined shouldBe false
            newResult.subExecutions.size shouldBe 1
            newResult.subExecutions(VariableName("@@anonymous_3@@")).variables.map(_.name.name) shouldBe Seq("My Variable 2")
            newResult.agreements.size shouldBe 1

            parser.forReview(newResult.agreements.head,ParagraphEdits()) shouldBe "<p class='no-section'>it is just another template hello</p>"
          case Left(ex) =>
            fail(ex)
        }

      case Left(ex) =>
        fail(ex)
    }
  }

  it should "detect cyclic dependencies" in {
    val text =
      """
        |<%
        |[[My Variable:Text]]
        |[[Other one:Number]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
        |
        |[[My Template: Template("Another Template")]]
      """.stripMargin

    val text2 = """[[template:Template("My Template")]]"""
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters = TemplateParameters()
    engine.execute(compiledTemplate, parameters, Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionWaitForTemplate(VariableName("My Template"), TemplateSourceIdentifier(TemplateTitle("Another Template")))
        result.variables.map(_.name.name) shouldBe Seq("My Variable", "Other one", "My Template")

        engine.resumeExecution(result, Map(TemplateSourceIdentifier(TemplateTitle("Another Template")) -> otherCompiledTemplate, TemplateSourceIdentifier(TemplateTitle("My Template")) -> compiledTemplate)) match {
          case Right(_) =>
            fail("should fail")
          case Left(ex) =>
            ex shouldBe "cyclic dependency detected on 'Another Template'"
        }

      case Left(ex) =>
        fail(ex)
    }
  }

  it should "detect variable type mismatch" in {
    val text =
      """
        |<%
        |[[My Variable:Text]]
        |[[Other one:Number]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
        |
        |[[My Template: Template("Another Template")]]
      """.stripMargin

    val text2 = """[[My Variable:Number]]"""
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters = TemplateParameters()
    engine.execute(compiledTemplate, parameters, Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionWaitForTemplate(VariableName("My Template"), TemplateSourceIdentifier(TemplateTitle("Another Template")))
        result.variables.map(_.name.name) shouldBe Seq("My Variable", "Other one", "My Template")

        engine.resumeExecution(result, Map(TemplateSourceIdentifier(TemplateTitle("Another Template")) -> otherCompiledTemplate, TemplateSourceIdentifier(TemplateTitle("My Template")) -> compiledTemplate)) match {
          case Right(_) =>
            fail("should fail")
          case Left(ex) =>
            ex shouldBe "Variable definition mismatch. variable My Variable is defined as Text in the main template but was Number in Another Template"
        }

      case Left(ex) =>
        fail(ex)
    }
  }

  it should "add one agreement in the execution result if the main template is an agreement" in {
    val text =
      """
        |<%
        |[[My Variable:Text]]
        |[[Other one:Number]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
        |
      """.stripMargin

    val compiledTemplate = compile(text)
    val parameters = TemplateParameters()
    engine.execute(compiledTemplate, parameters, Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        result.variables.map(_.name.name) shouldBe Seq("My Variable", "Other one")
        result.agreements.size shouldBe 1

        parser.forReview(result.agreements.head,ParagraphEdits()) shouldBe "<p class='no-section'><br/></p><p class='no-section'>[[My Variable]] - [[Other one]]<br/><br/>    </p>"

      case Left(ex) =>
        fail(ex)
    }
  }

  it should "see a variable as executed if it has been executed in a sub template" in {
    val mainTemplate =
      compile("""
        |<%
        |[[var]]
        |[[var 2]]
        |%>
        |
        |
        |[[template:Template(
        |name: "template";
        |parameters: var -> var
        |)]]
      """.stripMargin)

    val subTemplate = compile("[[var]] [[var 2]]")

    engine.execute(mainTemplate, TemplateParameters(), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)) match {
      case Right(result) =>
        result.getExecutedVariables.map(_.name) shouldBe Seq("template", "var", "var 2")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "map variable for sub templates" in {
    val mainTemplate =
      compile("""<%
                [[var]]
                [[template:Template(
                name: "template";
                parameters:
                  other var -> var + " world")]]
              %>

              [[template]]""".stripMargin)

    val subTemplate = compile("[[other var]]")

    engine.execute(mainTemplate, TemplateParameters("var" -> "Hello"), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)) match {
      case Right(result) =>
        parser.forReview(result.agreements.head,ParagraphEdits()) shouldBe "<p class='no-section'>Hello world</p>"
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "define path for sub templates" in {
    val mainTemplate =
      compile("""<%
                [[var]]
                [[template:Template(
                name: "template";
                path: var / "template" / "me";
                parameters:
                  other var -> var + " world")]]
              %>

              [[template]]""".stripMargin)

    val subTemplate = compile("[[other var]]")

    engine.execute(mainTemplate, TemplateParameters("var" -> "Hello"), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)) match {
      case Right(result) =>
        result.agreements.head.directory.path shouldBe Seq("Hello", "template", "me")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "define path for sub templates even if only one level" in {
    val mainTemplate =
      compile("""<%
                [[var]]
                [[template:Template(
                name: "template";
                path: "template";
                parameters:
                  other var -> var + " world")]]
              %>

              [[template]]""".stripMargin)

    val subTemplate = compile("[[other var]]")

    engine.execute(mainTemplate, TemplateParameters("var" -> "Hello"), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)) match {
      case Right(result) =>
        result.agreements.head.directory.path shouldBe Seq("template")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "see a variable as executed if only used in a template parameters only if the overridden variable is being executed too" in {
    val mainTemplate =
      compile("""<%
                [[var]]
              %>
              |[[template:Template(
 |                name: "template";
 |                parameters:
 |                  other var -> var + " world")]]""".stripMargin)

    val subTemplate = compile(
      """<%
         [[other var]]
         %>
         {{cond => [[other var]]}}
      """.stripMargin)

    engine.execute(mainTemplate, TemplateParameters("var" -> "Hello", "cond" -> "false"), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)) match {
      case Right(result) =>
        result.getExecutedVariables.map(_.name).toSet shouldBe Set("template", "cond")
      case Left(ex) =>
        fail(ex)
    }

    engine.execute(mainTemplate, TemplateParameters("var" -> "Hello", "cond" -> "true"), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)) match {
      case Right(result) =>
        result.getExecutedVariables.map(_.name).toSet shouldBe Set("template", "var", "cond")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "the variable or alias of a sub template should be accessible in the current one" in {
    val mainTemplate =
      compile("""[[template:Template("template")]]
              |
              |[[template.other var]]
              |[[template.alias]]
              |""".stripMargin)

    val subTemplate = compile(
      """[[other var]][[@alias = other var + " World"]]""".stripMargin)

    engine.execute(mainTemplate, TemplateParameters("other var" -> "Hello"), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)) match {
      case Right(result) =>
        result.getAllExecutedVariables.map({case (_, variable) => variable.name}).toSet shouldBe Set("template", "other var")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "be possible to format an alias" in {
    val mainTemplate =
      compile("""<%
                [[@var = "hello world"]]
              %>[[var | uppercase]]""".stripMargin)


    engine.execute(mainTemplate, TemplateParameters(), Map()) match {
      case Right(result) =>
        parser.forReview(result.agreements.head,ParagraphEdits()) shouldBe "<p class='no-section'>HELLO WORLD</p>"
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "define a collection" in {
    val mainTemplate =
      compile(
        """<%[[My Collection:Collection<Text>]]%>
          |
          |{{#for each title : My Collection =>
          | [[title]]
          |}}
        """.stripMargin)

    val collectionType = AbstractCollectionType.createParameterInstance(TextType)
    engine.execute(mainTemplate, TemplateParameters("title" -> "this is a test", "My Collection" -> collectionType.internalFormat(CollectionValue(size = 3, values = Map(0 -> "test1", 1 -> "test2", 2 -> "test3"), collectionType = collectionType))), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head,ParagraphEdits())
        text shouldBe "<p class='no-section'>test1<br/>test2<br/>test3<br/><br/>        </p>"
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "be able to take a variable from outside the for each a collection" in {
    val mainTemplate =
      compile(
        """<%[[My Collection:Collection<Text>]]
          |[[number:Number]]
          |[[title:Text]]
          |%>
          |
          |{{#for each title : My Collection =>
          | [[title]] - [[number]]
          |}}
        """.stripMargin)

    val collectionType = AbstractCollectionType.createParameterInstance(TextType)

    engine.execute(mainTemplate, TemplateParameters("number" -> "10", "title" -> "this is a test", "My Collection" -> collectionType.internalFormat(CollectionValue(size = 3, values = Map(0 -> "test1", 1 -> "test2", 2 -> "test3"), collectionType = collectionType))), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        result.getAllExecutedVariables.map({case (_, variable) => variable.name}).toSet should contain theSameElementsAs Set("@@anonymous_1@@","@@anonymous_5@@","@@anonymous_3@@", "title", "number", "My Collection")
        parser.forReview(result.agreements.head,ParagraphEdits()) shouldBe "<p class='no-section'>test1 - 10<br/>test2 - 10<br/>test3 - 10<br/><br/>        </p>"
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "be able to evaluate a structured type" in {
    val mainTemplate =
      compile(
        """[[Party:Structure(
           name:Text;
           number:Number
        )]][[Someone:Party]]
        """.stripMargin)

    engine.execute(mainTemplate, TemplateParameters("Someone" -> Json.obj("name" -> "David", "number" -> "23").toString()), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        result.getAllExecutedVariables.map({case (_, variable) => variable.name}).toSet should contain theSameElementsAs Set("Someone")
        result.getVariableValue[Map[VariableName, Any]](VariableName("Someone")) shouldBe Some(Map(VariableName("name") -> "David", VariableName("number") -> 23))
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "handle if you define a collection more than once" in {
    val mainTemplate =
      compile(
        """[[list:Collection<Text>]]
          |[[list:Collection<Text>]]
        """.stripMargin)

    engine.execute(mainTemplate, TemplateParameters(), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        result.getAllExecutedVariables.map({case (_, variable) => variable.name}).toSet should contain theSameElementsAs Set("list")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "not duplicate executed values in sub templates" in {
    val mainTemplate =
      compile("""
           [[_:Template(
            name:"template";
            parameters:
            contract -> "0x0aA7511BA4FE893a3d2D68F295eB052543Df9E9F",
            function -> "hello"
           )]]
           [[_:Template(name:"template2";
           parameters:
            contract -> "0x0aA7511BA4FE893a3d2D68F295eB052543Df9E9F",
            function -> "world"
            )]]
          """.stripMargin)

    val subTemplate = compile(
      """
        |[[contract:EthAddress]]
        |[[function:Text]]
        |[[call1:EthereumCall(contract:contract;
        |interface:"ipfs:djqiodjqwiodjqwiodjqwiodjqwiodj";
        |network:"4";
        |function:function;
        |repeatEvery:"1 minute";
        |parameters:10000)]]
      """.stripMargin)

    val subTemplate2 = compile(
      """
        |[[contract:EthAddress]]
        |[[function:Text]]
        |[[call2:EthereumCall(contract:contract;
        |interface:"ipfs:djqiodjqwiodjqwiodjqwiodjqwiodj";
        |network:"4";
        |function:function;
        |repeatEvery:"1 minute";
        |parameters:10000)]]
      """.stripMargin)

    engine.execute(mainTemplate, TemplateParameters(), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate, TemplateSourceIdentifier(TemplateTitle("template2")) -> subTemplate2)) match {
      case Right(result) =>
        result.getExecutedVariables.map(_.name).toSet shouldBe Set("@@anonymous_1@@", "@@anonymous_3@@","call1", "call2")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "not duplicate agreements in for each" in {
    val mainTemplate =
      compile("""[[employees:Collection<Text>]]
                 {{#for each e:employees =>
           [[_:Template(
            name:"template";
            parameters:
            contract -> "0x0aA7511BA4FE893a3d2D68F295eB052543Df9E9F",
            function -> "hello"
           )]]
           }}
          """.stripMargin)

    val subTemplate = compile(
      """[[_:Template("template2")]]""".stripMargin)

    val subTemplate2 = compile(
      """bla bla""".stripMargin)

    val colType = AbstractCollectionType.createParameterInstance(TextType)
    val internalValue = colType.internalFormat(CollectionValue(2, Map(0 -> "hello", 1 -> "world"), colType))
    engine.execute(mainTemplate, TemplateParameters("employees" -> internalValue), Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate,TemplateSourceIdentifier(TemplateTitle("template2")) -> subTemplate2)) match {
      case Right(result) =>
        result.agreements.size shouldBe 2
        result.getExecutedVariables.map(_.name).toSet shouldBe Set("employees", "@@anonymous_1@@", "@@anonymous_3@@", "@@anonymous_5@@", "@@anonymous_7@@", "@@anonymous_9@@","@@anonymous_11@@")
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "fail if it makes a divide by zero error" in {
    val template =
      compile("""
        |[[Var 1:Number]]
        |[[Var 2:Number]]
        |[[@Expr = Var 1 / Var 2]]
        |
        |[[Expr]]
      """.stripMargin)

    engine.execute(template, TemplateParameters("Var 1" -> "4", "Var 2" -> "0"), Map()) match {
      case Right(_) =>
        fail("should fail")
      case Left(ex) =>
        ex shouldBe "error while evaluating the expression 'Var 1/Var 2': division by zero!"
    }
  }

  it should "not generate the expression if it makes a divide by zero error with complex expression" in {
    val template =
      compile("""
        |[[Var 1:Number]]
        |[[Var 2:Number]]
        |[[@Expr = (Var 1 + Var 2) / (Var 1 - Var 2) ]]
        |
        |[[Expr]]f
      """.stripMargin)

    engine.execute(template, TemplateParameters("Var 1" -> "5", "Var 2" -> "5"), Map()) match {
      case Right(_) => fail("should fail")
      case Left(ex) =>
        ex shouldBe "error while evaluating the expression '(Var 1+Var 2)/(Var 1-Var 2)': division by zero!"
    }
  }

  it should "not cut off text when using for each" in {
    val template = compile (
      """
        |<%
        |[[Directors:Collection<Text> "List of Directors"]]
        |%>
        |
        |\centered**ACTION BY WRITTEN CONSENT OF**
        |**SOLE INCORPORATOR ****OF**
        |**[[Company Name | Uppercase]]**
        |
        |The undersigned, being the sole incorporator of **[[Company Name]]**, a Delaware corporation (the "***Company***"), pursuant to Section 108 of the Delaware General Corporation Law, adopts the following resolution by written consent:
        |
        |**Appointment of Directors**
        |
        |**Resolved,** that, effective as of this date, the following person is appointed an initial director of the Company to serve until the earliest of (i) the Company’s first annual meeting of stockholders, (ii) the due election and qualification of such director’s successor or (iii) such director’s death, resignation or removal:
        |
        |{{#for each Director:Directors => [[Director]]}}
      """.stripMargin)

    val collectionType = AbstractCollectionType.createParameterInstance(TextType)
    engine.execute(template, TemplateParameters("Directors" -> collectionType.internalFormat(CollectionValue(size = 3, values = Map(0 -> "test1", 1 -> "test2", 2 -> "test3"), collectionType = collectionType))), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head,ParagraphEdits())
        text shouldBe "<p class='no-section'><br/></p><p class='no-section align-center'><strong>ACTION BY WRITTEN CONSENT OF</strong><br/><strong>SOLE INCORPORATOR </strong><strong>OF</strong><br/><strong>[[Company Name]]</strong></p><p class='no-section'>The undersigned, being the sole incorporator of <strong>[[Company Name]]</strong>, a Delaware corporation (the \"<strong><em>Company</em></strong>\"), pursuant to Section 108 of the Delaware General Corporation Law, adopts the following resolution by written consent:</p><p class='no-section'><strong>Appointment of Directors</strong></p><p class='no-section'><strong>Resolved,</strong> that, effective as of this date, the following person is appointed an initial director of the Company to serve until the earliest of (i) the Company’s first annual meeting of stockholders, (ii) the due election and qualification of such director’s successor or (iii) such director’s death, resignation or removal:</p><p class='no-section'>test1test2test3<br/>      </p>"
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "handle choice properly in a conditional" in {
    val template = compile (
      """<%
        [[my choice:Choice("value 1","value 2")]]
        [[value:my choice]]
        %>{{value = "value 1" => hello}}
      """.stripMargin)

    engine.execute(template, TemplateParameters("value" -> "value 1"), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head,ParagraphEdits())
        text shouldBe "<p class='no-section'>hello<br/>      </p>"
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "show an error if an expression is using an unknown variable" in {
    val template = compile (
      """<%
        [[my choice:Choice("value 1","value 2")]]
        [[value:my choice]]
        %>{{valuess = "value 1" => hello}}
      """.stripMargin)


    engine.execute(template, TemplateParameters(), Map()) match {
      case Right(_) =>
        fail("should fail!")
      case Left(ex) =>
        ex shouldBe "valuess cannot be resolved!"
    }
  }

  it should "handle section correctly when within a conditional" in {
    val template =
      compile("""<%
        |==Other==
        |[[Additional Agreements:YesNo "Will the employee be signing additional agreements?"]]
        |[[Confidentiality Agreement:YesNo "A confidentiality agreement?"]]
        |[[Dispute Resolution:YesNo "An alternative dispute resolution agreement?"]]
        |[[Governing Law]]
        |%>
        |
        |^**Pre-employment Conditions.**
        |
        |{{Additional Agreements => ^^*Additional Agreements* Your acceptance of this offer and commencement of employment with the Company is contingent upon the execution, and delivery to an officer of the Company, prior to or on your Start Date, {{Confidentiality Agreement => the Company's Confidential Information and Invention Assignment Agreement (the "Confidentiality Agreement")}} {{(Confidentiality Agreement && Dispute Resolution) and}} {{Dispute Resolution => an Alternative Dispute Resolution Agreement}}.}}
        |
        |^^*Right to Work.* For purposes of
        |
        |^^*No Conflicting Obligations.* You understand and agree that by
      """.stripMargin)

      engine.execute(template, TemplateParameters("Additional Agreements" -> "true"), Map()) match {
        case Right(result) =>
          result.state shouldBe ExecutionFinished
          val text = parser.forReview(result.agreements.head,ParagraphEdits())
          text shouldBe "<ul class='list-lvl-1'><li><p>1. <strong>Pre-employment Conditions.</strong></p><ul class='list-lvl-2'><li><p>(a) <em>Additional Agreements</em> Your acceptance of this offer and commencement of employment with the Company is contingent upon the execution, and delivery to an officer of the Company, prior to or on your Start Date,   .</p></li><li><p>(b) <em>Right to Work.</em> For purposes of</p></li><li><p>(c) <em>No Conflicting Obligations.</em> You understand and agree that by<br/>      </p></li></ul></li></ul>"
        case Left(ex) =>
          fail(ex)
      }
  }

  it should "handle section correctly when within a for loop" in {
    val template =
      compile("""<%
                |==Other==
                |[[text:Collection<Text>]]
                |%>
                |^ ** some test **
                |{{#for each t : text =>
                |^^(name) ** [[t]] ** as mentioned in [[name]]
                |}}
                |
                |^(last(numbering:2)) **hello world ** [[last]]
              """.stripMargin)

    val collectionType = AbstractCollectionType.createParameterInstance(TextType)

    engine.execute(template, TemplateParameters("text" -> collectionType.internalFormat(CollectionValue(size = 3, values = Map(0 -> "hello", 1 -> "world", 2 -> "me"), collectionType = collectionType))), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head,ParagraphEdits())
        text shouldBe "<p class='no-section'><br/></p><ul class='list-lvl-1'><li><p>1.  <strong> some test </strong><br/></p><ul class='list-lvl-2'><li><p>(a)  <strong> hello </strong> as mentioned in 1.(a)<br/></p></li><li><p>(b)  <strong> world </strong> as mentioned in 1.(b)<br/></p></li><li><p>(c)  <strong> me </strong> as mentioned in 1.(c)<br/></p></li></ul></li><li><p>2.  <strong>hello world </strong> 2.<br/>              </p></li></ul>"
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "not break at the end of the document with sections" in {
    val template =
      compile("""^ hello
                |
                |finishing [[my]] friend""".stripMargin)


    engine.execute(template, TemplateParameters(), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head,ParagraphEdits())
        text shouldBe """<ul class='list-lvl-1'><li><p>1.  hello</p><p class='no-section'>finishing [[my]] friend</p></li></ul>"""
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "handle correctly resetting the sections even if the reset is being done in a sub level" in {
    val template =
      compile("""^ first section
                |
                |^ second section
                |
                |^^ first sub section
                |
                |^^ second sub section
                |
                |^^(my section(numbering:1)) reset the section
                |
                |^ go back to the section [[my section]]
                |
                |""".stripMargin)


    engine.execute(template, TemplateParameters(), Map()) match {
      case Right(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head,ParagraphEdits())
        text shouldBe """<ul class='list-lvl-1'><li><p>1.  first section</p></li><li><p>2.  second section</p><ul class='list-lvl-2'><li><p>(a)  first sub section</p></li><li><p>(b)  second sub section</p></li><li><p>(a)  reset the section</p></li></ul></li><li><p>3.  go back to the section 2.(a)</p></li></ul>"""
      case Left(ex) =>
        fail(ex)
    }
  }

  private def compile(text:String):CompiledTemplate = parser.compileTemplate(text) match {
    case Right(template) => template
    case Left(ex) =>
      fail(ex)
  }
}
