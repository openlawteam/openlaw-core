package org.adridadou.openlaw.vm

import java.time.{Clock, Instant, ZonedDateTime}
import java.time.temporal.ChronoUnit

import org.adridadou.openlaw.result.Implicits.failureCause2Exception
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.Formatter
import org.adridadou.openlaw.parser.template.variableTypes.{VariableType, _}
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.result.Implicits.RichResult
import org.adridadou.openlaw.values.{TemplateParameters, TemplateTitle}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import play.api.libs.json.Json
import org.adridadou.openlaw.{OpenlawMap, _}
import io.circe.syntax._
import io.circe.parser._

class OpenlawExecutionEngineSpec extends FlatSpec with Matchers {

  val parser = new OpenlawTemplateLanguageParserService()
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
    val parameters =
      TemplateParameters("My Variable 2" -> "hello", "Other one" -> "334")
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.variables.map(_.name.name) shouldBe List(
          "My Variable",
          "Other one"
        )
      case Failure(ex, message) => fail(message, ex)
    }
  }

  it should "run a simple template with large text" in {
    val text =
      """
        |<%
        |[[My Variable:LargeText]]
        |[[Other one:Number]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
      """.stripMargin

    val compiledTemplate = compile(text)
    val parameters =
      TemplateParameters("My Variable 2" -> "hello", "Other one" -> "334")
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.variables.map(_.name.name) shouldBe List(
          "My Variable",
          "Other one"
        )
      case Left(ex) => fail(ex.message, ex)
    }
  }

  it should "handle event filter properly" in {
    val abi =
      """[{"constant":false,"inputs":[{"name":"_spender","type":"address"},{"name":"_value","type":"uint256"}],"name":"ContractCreation","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[],"name":"registerTokenLaunch","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":true,"name":"to","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"owner","type":"address"},{"indexed":true,"name":"spender","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Approval","type":"event"}]"""
    val template = compile(s"""[[Id:Identity]]
                  [[Employer Ethereum Address:EthAddress]]

                  [[Contract Creation Event: EthereumEventFilter(
                  contract address: "0x531E0957391dAbF46f8a9609d799fFD067bDbbC0";
                  interface: $abi;
                  event type name: "Approval";
                  conditional filter: this.event.owner = Employer Ethereum Address)]]

      [[some address:EthAddress]]

      {{Contract Creation Event.event.owner = some address => hello world}}
      """)

    engine.execute(template) match {
      case Success(executionResult) =>
        val text = parser.forReview(executionResult.agreements.head)
        text shouldBe "<p class=\"no-section\"><br />                  [[Employer Ethereum Address]]<br /><br />                </p><p class=\"no-section\"><br /><br />    </p><p class=\"no-section\">[[some address]]<br /><br />    </p><p class=\"no-section\"><br />      </p>"

      case Failure(ex, message) =>
        fail(message, ex)
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
        |[[my clause:Clause("Another Template")]]
      """.stripMargin

    val text2 = "it is just another template [[My Variable 2:Text]]"
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters =
      TemplateParameters("My Variable 2" -> "hello", "Other one" -> "334")
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionWaitForTemplate(
          VariableName("my clause"),
          TemplateSourceIdentifier(TemplateTitle("Another Template")),
          executionType = ClauseExecution
        )
        result.variables.map(_.name.name) shouldBe List(
          "My Variable",
          "@@anonymous_1@@",
          "@@anonymous_2@@",
          "Other one",
          "my clause"
        )

        engine.resumeExecution(
          result,
          Map(
            TemplateSourceIdentifier(TemplateTitle("another Template")) -> otherCompiledTemplate
          )
        ) match {
          case Success(newResult) =>
            newResult.state shouldBe ExecutionFinished
            newResult.parentExecution.isDefined shouldBe false
            newResult.subExecutions.size shouldBe 1
            newResult
              .subExecutions(VariableName("my clause"))
              .getVariables
              .map(_.name.name) shouldBe List("My Variable 2")
            newResult.agreements.size shouldBe 1

            val jsonResult = newResult.toSerializable.asJson.noSpaces
            decode[SerializableTemplateExecutionResult](jsonResult).right.value.validateExecution
              .getOrThrow()

            parser.forReview(newResult.agreements.head) shouldBe """<p class="no-section"><br /></p><p class="no-section">[[My Variable]] - 334</p><p class="no-section">it is just another template hello<br />      </p>"""
          case Failure(ex, message) =>
            fail(message, ex)
        }

      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "wait for a clause and then finish its execution" in {
    val text =
      """
        |<%
        |[[My Variable:Text]]
        |[[Other one:Number]]
        |[[County of Venue:Text]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
        |
        |[[_:Clause("A Clause")]]
      """.stripMargin

    val text2 =
      """
        |**Choice of Law and Venue.** The parties agree that this Agreement is to be governed by and construed under the law of the State of [[State of Governing Law]] without regard to its conflicts of law provisions. The parties further agree that all disputes shall be resolved exclusively in state or federal court in [[County of Venue]], [[State of Venue]].
        |
        |it is just another template [[My Variable 2:Text]]""".stripMargin
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters =
      TemplateParameters("My Variable 2" -> "hello", "Other one" -> "334")

    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionWaitForTemplate(
          VariableName("@@anonymous_1@@"),
          TemplateSourceIdentifier(TemplateTitle("a clause")),
          executionType = ClauseExecution
        )
        engine.resumeExecution(
          result,
          Map(
            TemplateSourceIdentifier(TemplateTitle("A Clause")) -> otherCompiledTemplate
          )
        ) match {
          case Success(newResult) =>
            newResult.state shouldBe ExecutionFinished
            newResult.subExecutions.size shouldBe 1
            newResult.variables.map(_.name) should contain allElementsOf List(
              VariableName("My Variable"),
              VariableName("Other one"),
              VariableName("State of Governing Law"),
              VariableName("County of Venue"),
              VariableName("State of Venue")
            )
            newResult.executedVariables should contain allElementsOf List(
              VariableName("My Variable"),
              VariableName("Other one"),
              VariableName("State of Governing Law"),
              VariableName("County of Venue"),
              VariableName("State of Venue")
            )
            parser.forReview(newResult.agreements.head) shouldBe "<p class=\"no-section\"><br /></p><p class=\"no-section\">[[My Variable]] - 334</p><p class=\"no-section\"><br /><strong>Choice of Law and Venue.</strong> The parties agree that this Agreement is to be governed by and construed under the law of the State of [[State of Governing Law]] without regard to its conflicts of law provisions. The parties further agree that all disputes shall be resolved exclusively in state or federal court in [[County of Venue]], [[State of Venue]].</p><p class=\"no-section\">it is just another template hello<br />      </p>"
          case Failure(ex, message) =>
            fail(message, ex)
        }

      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "wait for a clause and then finish its execution and mark relevant variables as executed in mapping" in {
    val text =
      """
        |<%
        |[[My Variable:Text]]
        |[[Other one:Number]]
        |[[County of Venue 2:Text]]
        |%>
        |
        |[[My Variable]] - [[Other one]]
        |
        |[[_:Clause(
        |name:"A Clause";
        |parameters:
        |County of Venue -> County of Venue 2
        |)]]
      """.stripMargin

    val text2 =
      """
        |**Choice of Law and Venue.** The parties agree that this Agreement is to be governed by and construed under the law of the State of [[State of Governing Law]] without regard to its conflicts of law provisions. The parties further agree that all disputes shall be resolved exclusively in state or federal court in [[County of Venue]], [[State of Venue]].
        |
        |it is just another template [[My Variable 2:Text]]""".stripMargin
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters =
      TemplateParameters("My Variable 2" -> "hello", "Other one" -> "334")

    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionWaitForTemplate(
          VariableName("@@anonymous_1@@"),
          TemplateSourceIdentifier(TemplateTitle("a clause")),
          executionType = ClauseExecution
        )
        engine.resumeExecution(
          result,
          Map(
            TemplateSourceIdentifier(TemplateTitle("A Clause")) -> otherCompiledTemplate
          )
        ) match {
          case Success(newResult) =>
            newResult.state shouldBe ExecutionFinished
            newResult.subExecutions.size shouldBe 1
            newResult.variables.map(_.name) should contain allElementsOf Seq(
              VariableName("My Variable"),
              VariableName("Other one"),
              VariableName("State of Governing Law"),
              VariableName("County of Venue 2"),
              VariableName("State of Venue")
            )
            newResult.executedVariables should contain allElementsOf Seq(
              VariableName("My Variable"),
              VariableName("Other one"),
              VariableName("State of Governing Law"),
              VariableName("County of Venue 2"),
              VariableName("State of Venue")
            )
            parser.forReview(newResult.agreements.head) shouldBe "<p class=\"no-section\"><br /></p><p class=\"no-section\">[[My Variable]] - 334</p><p class=\"no-section\"><br /><strong>Choice of Law and Venue.</strong> The parties agree that this Agreement is to be governed by and construed under the law of the State of [[State of Governing Law]] without regard to its conflicts of law provisions. The parties further agree that all disputes shall be resolved exclusively in state or federal court in [[County of Venue]], [[State of Venue]].</p><p class=\"no-section\">it is just another template hello<br />      </p>"
          case Failure(ex, message) =>
            fail(message, ex)
        }

      case Failure(ex, message) =>
        fail(message, ex)
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
        |[[My Template: Clause("Another Template")]]
      """.stripMargin

    val text2 = """[[template:Clause("My Template")]]"""
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters = TemplateParameters()
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionWaitForTemplate(
          VariableName("My Template"),
          TemplateSourceIdentifier(TemplateTitle("Another Template")),
          executionType = ClauseExecution
        )
        result.variables.map(_.name.name) shouldBe Seq(
          "My Variable",
          "Other one",
          "My Template"
        )

        engine.resumeExecution(
          result,
          Map(
            TemplateSourceIdentifier(TemplateTitle("Another Template")) -> otherCompiledTemplate,
            TemplateSourceIdentifier(TemplateTitle("My Template")) -> compiledTemplate
          )
        ) match {
          case Success(_) =>
            fail("should fail")
          case Failure(_, message) =>
            message shouldBe "cyclic dependency detected on 'another template'"
        }

      case Failure(ex, message) =>
        fail(message, ex)
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
        |[[My Template: Clause("Another Template")]]
      """.stripMargin

    val text2 = """[[My Variable:Number]]"""
    val compiledTemplate = compile(text)
    val otherCompiledTemplate = compile(text2)
    val parameters = TemplateParameters()
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionWaitForTemplate(
          VariableName("My Template"),
          TemplateSourceIdentifier(TemplateTitle("Another Template")),
          executionType = ClauseExecution
        )
        result.variables.map(_.name.name) shouldBe Seq(
          "My Variable",
          "Other one",
          "My Template"
        )

        engine.resumeExecution(
          result,
          Map(
            TemplateSourceIdentifier(TemplateTitle("Another Template")) -> otherCompiledTemplate,
            TemplateSourceIdentifier(TemplateTitle("My Template")) -> compiledTemplate
          )
        ) match {
          case Success(_) =>
            fail("should fail")
          case Failure(_, message) =>
            message shouldBe "Variable definition mismatch. variable 'My Variable' is defined as 'Text' in 'the main template' but was 'Number' in 'another template'"
        }

      case Failure(ex, message) =>
        fail(message, ex)
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
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.variables.map(_.name.name) shouldBe Seq(
          "My Variable",
          "Other one"
        )
        result.agreements.size shouldBe 1

        parser.forReview(result.agreements.head) shouldBe """<p class="no-section"><br /></p><p class="no-section">[[My Variable]] - [[Other one]]<br /><br />    </p>"""

      case Failure(ex, message) =>
        fail(message, ex)
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
        |[[template:Clause(
        |name: "template";
        |parameters: var -> var
        |)]]
      """.stripMargin)

    val subTemplate = compile("[[var]] [[var 2]]")

    engine.execute(
      mainTemplate,
      TemplateParameters(),
      Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)
    ) match {
      case Success(result) =>
        result.getExecutedVariables.map(_.name) shouldBe Seq(
          "template",
          "var",
          "var 2"
        )
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "see a variable as executed if it has been executed in a clause" in {
    val mainTemplate =
      compile("""
                |<%
                |[[var]]
                |[[var 2]]
                |%>
                |
                |
                |[[clause:Clause(
                |name: "clause";
                |parameters: sub var -> var
                |)]]
              """.stripMargin)

    val subTemplate = compile("[[sub var]] [[var 2]]")

    engine.execute(
      mainTemplate,
      TemplateParameters("var" -> "hello", "var 2" -> "world"),
      Map(TemplateSourceIdentifier(TemplateTitle("clause")) -> subTemplate)
    ) match {
      case Success(result) =>
        result.getExecutedVariables.map(_.name) shouldBe Seq(
          "clause",
          "var",
          "var 2"
        )
        parser.forReview(result.agreements.head) shouldBe "<p class=\"no-section\"><br /><br /></p><p class=\"no-section\">hello world<br />              </p>"
      case Left(ex) =>
        fail(ex.message, ex)
    }
  }

  it should "map variable for sub templates" in {
    val mainTemplate =
      compile("""<%
                [[var]]
                [[template:Clause(
                name: "template";
                parameters:
                  other var -> var + " world")]]
              %>[[template]]""".stripMargin)

    val subTemplate = compile("[[other var]]")

    engine.execute(
      mainTemplate,
      TemplateParameters("var" -> "Hello"),
      Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)
    ) match {
      case Success(result) =>
        parser.forReview(result.agreements.head) shouldBe """<p class="no-section">Hello world</p>"""
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "see a variable as executed if only used in a template parameters only if the overridden variable is being executed too" in {
    val mainTemplate =
      compile("""<%
                [[var]]
              %>
              |[[template:Clause(
 |                name: "template";
 |                parameters:
 |                  other var -> var + " world")]]""".stripMargin)

    val subTemplate = compile("""<%
         [[other var]]
         %>
         {{cond => [[other var]]}}
      """.stripMargin)

    engine.execute(
      mainTemplate,
      TemplateParameters("var" -> "Hello", "cond" -> "false"),
      Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)
    ) match {
      case Success(result) =>
        result.getExecutedVariables.map(_.name).toSet shouldBe Set(
          "template",
          "cond"
        )
      case Failure(ex, message) =>
        fail(message, ex)
    }

    engine.execute(
      mainTemplate,
      TemplateParameters("var" -> "Hello", "cond" -> "true"),
      Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)
    ) match {
      case Success(result) =>
        result.getExecutedVariables.map(_.name).toSet shouldBe Set(
          "template",
          "var",
          "cond"
        )
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "the variable or alias of a sub template should be accessible in the current one" in {
    val mainTemplate =
      compile("""[[template:Clause("template")]]
              |
              |[[template.other var]]
              |[[template.alias]]
              |""".stripMargin)

    val subTemplate =
      compile("""[[other var]][[@alias = other var + " World"]]""".stripMargin)

    engine.execute(
      mainTemplate,
      TemplateParameters("other var" -> "Hello"),
      Map(TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate)
    ) match {
      case Success(result) =>
        result.getAllExecutedVariables
          .map({ case (_, variable) => variable.name })
          .toSet shouldBe Set("template", "other var")
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "be possible to format an alias" in {
    val mainTemplate =
      compile("""<%
                [[@var = "hello world"]]
              %>[[var | uppercase]]""".stripMargin)

    engine.execute(mainTemplate) match {
      case Success(result) =>
        parser.forReview(result.agreements.head) shouldBe """<p class="no-section">HELLO WORLD</p>"""
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "print a period properly" in {
    val mainTemplate =
      compile("""[[var:Period]]""".stripMargin)
    val result = engine
      .execute(
        mainTemplate,
        TemplateParameters(
          "var" -> PeriodType
            .internalFormat(PeriodType.cast("3 minute 10 seconds").right.value)
            .getOrThrow()
        )
      )
      .getOrThrow()
    parser.forReview(result.agreements.head) shouldBe """<p class="no-section">3 minutes 10 seconds</p>"""
  }

  it should "print a period containing a singular value properly" in {
    val mainTemplate =
      compile("""[[var:Period]]""".stripMargin)
    val result = engine
      .execute(
        mainTemplate,
        TemplateParameters(
          "var" -> PeriodType
            .internalFormat(PeriodType.cast("1 minute 1 second").getOrThrow())
            .getOrThrow()
        )
      )
      .getOrThrow()
    parser.forReview(result.agreements.head) shouldBe """<p class="no-section">1 minute 1 second</p>"""
  }

  it should "print a period containing a singular and non-singular value properly" in {
    val mainTemplate =
      compile("""[[var:Period]]""".stripMargin)
    val result = engine
      .execute(
        mainTemplate,
        TemplateParameters(
          "var" -> PeriodType
            .internalFormat(PeriodType.cast("1 minute 20 seconds").getOrThrow())
            .getOrThrow()
        )
      )
      .getOrThrow()
    parser.forReview(result.agreements.head) shouldBe """<p class="no-section">1 minute 20 seconds</p>"""
  }

  it should "define a collection" in {
    val mainTemplate =
      compile("""<%[[My Collection:Collection<Text>]]%>
          |
          |{{#for each title : My Collection =>
          | [[title]]
          |}}
        """.stripMargin)

    val collectionType =
      AbstractCollectionType.createParameterInstance(TextType)
    engine.execute(
      mainTemplate,
      TemplateParameters(
        "title" -> "this is a test",
        "My Collection" -> collectionType
          .internalFormat(
            CollectionValue(
              size = 3,
              values = Map(0 -> "test1", 1 -> "test2", 2 -> "test3"),
              collectionType = collectionType
            )
          )
          .getOrThrow()
      )
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<p class="no-section">test1<br />test2<br />test3<br /><br />        </p>"""
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "not modify dates in a collection when accessing values" in {
    val mainTemplate =
      compile("""<%[[My Collection:Collection<Date>]]%>
          |
          |{{#for each title : My Collection =>
          | [[title]]
          |}}
        """.stripMargin)

    val collectionType =
      AbstractCollectionType.createParameterInstance(DateType)

    val result = engine
      .execute(
        mainTemplate,
        TemplateParameters(
          "title" -> "this is a test",
          "My Collection" -> collectionType
            .internalFormat(
              CollectionValue(
                size = 0,
                values = Map.empty,
                collectionType = collectionType
              )
            )
            .getOrThrow()
        )
      )
      .getOrThrow()
    val executionResult = result.getAllExecutionResults.head

    val initialValue = "1564660800000"
    val initialCollection = s"""{"values":{"0":"$initialValue"},"size":2}"""
    val newValue = "1564660800000"
    val newCollection =
      s"""{"values":{"0":"$initialValue","1":"$newValue"},"size":2}"""

    val variable = result.getVariable("My Collection").value
    val varType = variable.varType(executionResult)
    val collection = VariableType
      .convert[CollectionValue](
        varType.cast(initialCollection, executionResult).getOrThrow()
      )
      .getOrThrow()
    initialValue should be(
      collection.valueInternalFormat(collection.list.head).right.value
    )

    val openlawValue =
      collection.castValue(newValue, executionResult).getOrThrow()
    val values: Map[Int, OpenlawValue] = collection.values ++ Map(
      1 -> openlawValue
    )
    val collectionOutput = collection.collectionType
      .internalFormat(collection.copy(values = values))
      .getOrThrow()

    collectionOutput should be(newCollection)
  }

  it should "be able to take a variable from outside the for each a collection" in {
    val mainTemplate =
      compile("""<%[[My Collection:Collection<Text>]]
          |[[number:Number]]
          |[[title:Text]]
          |%>
          |
          |{{#for each title : My Collection =>
          | [[title]] - [[number]]
          |}}
        """.stripMargin)

    val collectionType =
      AbstractCollectionType.createParameterInstance(TextType)

    engine.execute(
      mainTemplate,
      TemplateParameters(
        "number" -> "10",
        "title" -> "this is a test",
        "My Collection" -> collectionType
          .internalFormat(
            CollectionValue(
              size = 3,
              values = Map(0 -> "test1", 1 -> "test2", 2 -> "test3"),
              collectionType = collectionType
            )
          )
          .right
          .value
      )
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.getAllExecutedVariables
          .map({ case (_, variable) => variable.name })
          .toSet should contain theSameElementsAs Set(
          "@@anonymous_1@@",
          "@@anonymous_5@@",
          "@@anonymous_3@@",
          "title",
          "number",
          "My Collection"
        )
        parser.forReview(result.agreements.head) shouldBe """<p class="no-section">test1 - 10<br />test2 - 10<br />test3 - 10<br /><br />        </p>"""
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "be able to evaluate a structured type" in {
    val mainTemplate =
      compile("""[[Party:Structure(
           name:Text;
           number:Number
        )]][[Someone:Party]]
        """.stripMargin)

    engine.execute(
      mainTemplate,
      TemplateParameters(
        "Someone" -> Json.obj("name" -> "David", "number" -> "23").toString()
      )
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.getAllExecutedVariables
          .map({ case (_, variable) => variable.name })
          .toSet should contain theSameElementsAs Set("Someone")

        val map = result
          .getVariableValue[OpenlawMap[VariableName, OpenlawValue]](
            VariableName("Someone")
          )
          .right
          .value
          .value
          .underlying
        map.get(VariableName("name")).value.toString shouldBe "David"
        map
          .get(VariableName("number"))
          .map { case OpenlawBigDecimal(v) => v }
          .value shouldBe BigDecimal(23)
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "handle if you define a collection more than once" in {
    val mainTemplate =
      compile("""[[list:Collection<Text>]]
          |[[list:Collection<Text>]]
        """.stripMargin)

    engine.execute(mainTemplate) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.getAllExecutedVariables
          .map({ case (_, variable) => variable.name })
          .toSet should contain theSameElementsAs Set("list")
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "not duplicate executed values in sub templates" in {
    val mainTemplate =
      compile("""
           [[my clause:Clause(
            name:"template";
            parameters:
            contract -> "0x0aA7511BA4FE893a3d2D68F295eB052543Df9E9F",
            function -> "hello"
           )]]
           [[my second clause:Clause(name:"template2";
           parameters:
            contract -> "0x0aA7511BA4FE893a3d2D68F295eB052543Df9E9F",
            function -> "world"
            )]]
          """.stripMargin)

    val subTemplate = compile("""
        |[[contract:EthAddress]]
        |[[function:Text]]
        |[[call1:EthereumCall(contract:contract;
        |interface:"ipfs:djqiodjqwiodjqwiodjqwiodjqwiodj";
        |network:"4";
        |function:function;
        |repeatEvery:"1 minute";
        |parameters:10000)]]
      """.stripMargin)

    val subTemplate2 =
      compile("""
        |[[contract:EthAddress]]
        |[[function:Text]]
        |[[call2:EthereumCall(contract:contract;
        |interface:"ipfs:djqiodjqwiodjqwiodjqwiodjqwiodj";
        |network:"4";
        |function:function;
        |repeatEvery:"1 minute";
        |parameters:10000)]]
      """.stripMargin)

    engine.execute(
      mainTemplate,
      TemplateParameters(),
      Map(
        TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate,
        TemplateSourceIdentifier(TemplateTitle("template2")) -> subTemplate2
      )
    ) match {
      case Success(result) =>
        result.getExecutedVariables.map(_.name).toSet shouldBe Set(
          "my clause",
          "my second clause",
          "call1",
          "call2"
        )
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "not duplicate agreements in for each" in {
    val mainTemplate =
      compile("""[[employees:Collection<Text>]]
                 {{#for each e:employees =>
           [[_:Clause(
            name:"template";
            parameters:
            contract -> "0x0aA7511BA4FE893a3d2D68F295eB052543Df9E9F",
            function -> "hello"
           )]]
           }}
          """.stripMargin)

    val subTemplate =
      compile("""[[_:Clause("template2")]]""".stripMargin)

    val subTemplate2 = compile("""bla bla""".stripMargin)

    val colType = AbstractCollectionType.createParameterInstance(TextType)
    val internalValue = colType
      .internalFormat(
        CollectionValue(2, Map(0 -> "hello", 1 -> "world"), colType)
      )
      .getOrThrow()

    engine.execute(
      mainTemplate,
      TemplateParameters("employees" -> internalValue),
      Map(
        TemplateSourceIdentifier(TemplateTitle("template")) -> subTemplate,
        TemplateSourceIdentifier(TemplateTitle("template2")) -> subTemplate2
      )
    ) match {
      case Success(result) =>
        result.getExecutedVariables.map(_.name).toSet shouldBe Set(
          "employees",
          "@@anonymous_1@@",
          "@@anonymous_3@@"
        )
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "look at sub execution results for variable type" in {
    val mainTemplate = compile("""[[c:Clause("clause")]]""")
    val clauseTemplate = compile(
      """
        |<%
        |# Arranged Variables by Header
        |==MOU Terms==
        |[[Terms: Collection<Text>]]
        |
        |==MOU Parties==
        |[[Party Info: Structure(
        |    Party Name: Text;
        |    Party Email: Identity
        |    )]]
        |[[#Parties: Collection<Party Info>]]
        |%>
        |
        |\centered **__MEMORANDUM OF UNDERSTANDING__**
        |
        |**NOW, THEREFORE,** the undersigned parties (the "***Parties***") see mutual benefit in entering into a partnership and formalizing their cooperation over the matters and under the terms sufficiently described in this Memorandum of Understanding ("***MOU***"):
        |
        |{{#for each Term: Terms =>
        |
        |    ^^[[Term]]
        |}}
        |
        |The Parties shall keep confidential and shall not divulge to any other party, without the other's prior written consent, any non-public information concerning this MOU and the cooperation contemplated hereby, treating such confidential information with the same due care as their own proprietary information.
        |
        |The Parties shall jointly operate and exchange information with the intention of completing the transactions contemplated hereby and within determined timeframes and reasonable best efforts.
        |
        |After a preliminary review period, not to exceed two (2) months from the date of mutual execution below, the Parties may enter into a legally-binding agreement regarding these matters and shall make reasonable best efforts to conduct a meeting regarding the same following such MOU review period.
        |
        |This MOU is not a legally-binding agreement.
        |
        |***__SIGNATORIES__***
        |
        |{{#for each Party: Parties =>
        |
        |**[[Party.Party Name | Uppercase]]**
        |
        |*__[[Party.Party Email | Signature]]__*
        |Authorized Representative
        |
        |}}
        |""".stripMargin
    )

    engine.execute(
      mainTemplate,
      TemplateParameters(),
      Map(
        TemplateSourceIdentifier(TemplateTitle("clause")) -> clauseTemplate,
        TemplateSourceIdentifier(TemplateTitle("clause")) -> clauseTemplate
      )
    ) match {
      case Success(result) =>
        val variable = VariableDefinition(
          VariableName("Parties"),
          Some(
            VariableTypeDefinition(
              "Collection",
              Some(VariableTypeDefinition("Parefhuzegfty Info", None))
            )
          )
        )
        getCollection(variable, result, "")
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  def getCollection(
      variable: VariableDefinition,
      executionResult: TemplateExecutionResult,
      value: String
  ): CollectionValue = {
    variable.varType(executionResult) match {
      case collectionType: CollectionType =>
        if (value.isEmpty) {
          CollectionValue(collectionType = collectionType)
        } else {
          VariableType
            .convert[CollectionValue](
              collectionType.cast(value, executionResult).getOrThrow()
            )
            .getOrThrow()
        }
      case other =>
        throw new RuntimeException(
          s"add element to collection only works for a variable of type Collection, not '${other.name}'"
        )
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

    engine.execute(
      template,
      TemplateParameters("Var 1" -> "4", "Var 2" -> "0")
    ) match {
      case Right(_) =>
        fail("should fail")
      case Left(ex) =>
        ex.message shouldBe "error while evaluating the expression 'Var 1 / Var 2': division by zero!"
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

    engine.execute(
      template,
      TemplateParameters("Var 1" -> "5", "Var 2" -> "5")
    ) match {
      case Right(_) => fail("should fail")
      case Left(ex) =>
        ex.message shouldBe "error while evaluating the expression '(Var 1 + Var 2) / (Var 1 - Var 2)': division by zero!"
    }
  }

  it should "not cut off text when using for each" in {
    val template = compile(
      """
        |<%
        |[[Directors:Collection<Text> "List of Directors"]]
        |%>
        |
        |\centered**ACTION BY WRITTEN CONSENT OF**
        |**SOLE INCORPORATOR ****OF**
        |**[[Company Name | Uppercase]]**
        |
        |The undersigned __Name__, being the sole incorporator of **[[Company Name]]**, a Delaware corporation (the "***Company***"), pursuant to Section 108 of the Delaware General Corporation Law, adopts the following resolution by written consent:
        |
        |**Appointment of Directors**
        |
        |**Resolved,** that, effective as of this date, the following person is appointed an initial director of the Company to serve until the earliest of (i) the Company’s first annual meeting of stockholders, (ii) the due election and qualification of such director’s successor or (iii) such director’s death, resignation or removal:
        |
        |{{#for each Director:Directors => [[Director]]}}
      """.stripMargin
    )

    val collectionType =
      AbstractCollectionType.createParameterInstance(TextType)
    engine.execute(
      template,
      TemplateParameters(
        "Directors" -> collectionType
          .internalFormat(
            CollectionValue(
              size = 3,
              values = Map(0 -> "test1", 1 -> "test2", 2 -> "test3"),
              collectionType = collectionType
            )
          )
          .right
          .value
      )
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<p class="no-section"><br /></p><p class="align-center no-section"><strong>ACTION BY WRITTEN CONSENT OF</strong><br /><strong>SOLE INCORPORATOR </strong><strong>OF</strong><br /><strong>[[Company Name]]</strong></p><p class="no-section">The undersigned <u>Name</u>, being the sole incorporator of <strong>[[Company Name]]</strong>, a Delaware corporation (the &quot;<strong><em>Company</em></strong>&quot;), pursuant to Section 108 of the Delaware General Corporation Law, adopts the following resolution by written consent:</p><p class="no-section"><strong>Appointment of Directors</strong></p><p class="no-section"><strong>Resolved,</strong> that, effective as of this date, the following person is appointed an initial director of the Company to serve until the earliest of (i) the Company’s first annual meeting of stockholders, (ii) the due election and qualification of such director’s successor or (iii) such director’s death, resignation or removal:</p><p class="no-section">test1test2test3<br />      </p>"""
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "handle choice properly in a conditional" in {
    val template = compile("""<%
        [[my choice:Choice("value 1","value 2")]]
        [[value:my choice]]
        %>{{value = "value 1" => hello}}
      """.stripMargin)

    engine.execute(template, TemplateParameters("value" -> "value 1")) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<p class="no-section">hello<br />      </p>"""
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "show an error if an expression is using an unknown variable" in {
    val template = compile("""<%
        [[my choice:Choice("value 1","value 2")]]
        [[value:my choice]]
        %>{{values = "value 1" => hello}}
      """.stripMargin)

    engine.execute(template, TemplateParameters()) match {
      case Right(_) =>
        fail("should fail!")
      case Left(ex) =>
        ex.message shouldBe "values cannot be resolved!"
    }
  }

  it should "handle section correctly when within a conditional" in {
    val template =
      compile(
        """<%
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
      """.stripMargin
      )

    engine.execute(
      template,
      TemplateParameters("Additional Agreements" -> "true")
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<ul class="list-lvl-1"><li><p>1. <strong>Pre-employment Conditions.</strong></p><p></p><ul class="list-lvl-2"><li><p>(a) <em>Additional Agreements</em> Your acceptance of this offer and commencement of employment with the Company is contingent upon the execution, and delivery to an officer of the Company, prior to or on your Start Date,   .</p></li><li><p>(b) <em>Right to Work.</em> For purposes of</p></li><li><p>(c) <em>No Conflicting Obligations.</em> You understand and agree that by<br />      </p></li></ul></li></ul>"""
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

    val collectionType =
      AbstractCollectionType.createParameterInstance(TextType)

    engine.execute(
      template,
      TemplateParameters(
        "text" -> collectionType
          .internalFormat(
            CollectionValue(
              size = 3,
              values = Map(0 -> "hello", 1 -> "world", 2 -> "me"),
              collectionType = collectionType
            )
          )
          .right
          .value
      )
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<p class="no-section"><br /></p><ul class="list-lvl-1"><li><p>1.  <strong> some test </strong><br /></p><ul class="list-lvl-2"><li><p>(a)  <strong> hello </strong> as mentioned in 1.a<br /></p></li><li><p>(b)  <strong> world </strong> as mentioned in 1.b<br /></p></li><li><p>(c)  <strong> me </strong> as mentioned in 1.c<br /></p></li></ul></li><li><p>2.  <strong>hello world </strong> 2<br />              </p></li></ul>"""
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "not break at the end of the document with sections" in {
    val template =
      compile("""^ hello
                |
                |finishing [[my]] friend""".stripMargin)

    engine.execute(template, TemplateParameters()) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<ul class="list-lvl-1"><li><p>1.  hello</p><p>finishing [[my]] friend</p></li></ul>"""
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "give us the proper type for choices" in {
    val template = compile("""
        |[[Choice Type:Choice("hello","world")]]
        |
        |[[var:Choice Type]]
      """.stripMargin)

    engine.execute(template, TemplateParameters("var" -> "hello")) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val Some(variable) = result.getVariable("var")
        variable.varType(result).getTypeClass shouldBe classOf[OpenlawString]

        variable.evaluate(result).right.value.value.toString shouldBe "hello"

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

    engine.execute(template) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<ul class="list-lvl-1"><li><p>1.  first section</p></li><li><p>2.  second section</p><ul class="list-lvl-2"><li><p>(a)  first sub section</p></li><li><p>(b)  second sub section</p></li><li><p>(a)  reset the section</p></li></ul></li><li><p>3.  go back to the section 2.a</p></li></ul>"""
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "handle header level annotations" in {
    val startEndQuote = "\'\'\'"
    val template =
      compile(s"""
           |before the annotation
           |
          |$startEndQuote
           |this is some text for my annotation
           |$startEndQuote
           |
          |after the annotation
        """.stripMargin)

    engine.execute(template) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<p class="no-section"><br />before the annotation</p><p class="no-section"></p><p class="no-section">after the annotation<br />        </p>"""
        val text2 = parser.forPreview(result.agreements.head)
        text2 shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section"><br />before the annotation</p></div><div class="openlaw-paragraph paragraph-2"><p class="no-section"><span class="openlaw-annotation-header"><br />this is some text for my annotation<br /></span></p></div><div class="openlaw-paragraph paragraph-3"><p class="no-section">after the annotation<br />        </p></div>"""
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "handle note level annotations" in {
    val startEndQuote = "\"\"\""
    val template =
      compile(s"""
           |before the annotation
           |
          |$startEndQuote
           |this is some text for my annotation
           |$startEndQuote
           |
          |after the annotation
        """.stripMargin)

    engine.execute(template) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<p class="no-section"><br />before the annotation</p><p class="no-section"></p><p class="no-section">after the annotation<br />        </p>"""
        val text2 = parser.forPreview(result.agreements.head)
        text2 shouldBe """<div class="openlaw-paragraph paragraph-1"><p class="no-section"><br />before the annotation</p></div><div class="openlaw-paragraph paragraph-2"><p class="no-section"><span class="openlaw-annotation-note"><br />this is some text for my annotation<br /></span></p></div><div class="openlaw-paragraph paragraph-3"><p class="no-section">after the annotation<br />        </p></div>"""
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "fail if an option is not of the right type" in {
    val template =
      compile("""
           [[my var:Number]]
           [[my text:Text(
           options:"hello", my var
           )]]
        """.stripMargin)

    engine.execute(template) match {
      case Right(_) =>
        fail("execution should fail")
      case Left(ex) =>
        ex.message shouldBe "options element error! should be of type Text but my var is Number instead"
    }
  }

  it should "be possible to specify options for a choice type" in {
    val template =
      compile("""
           [[My Choice:Choice("one", "two", "three")]]
           [[my var:My Choice(
           options:"one", "two"
           )]]
        """.stripMargin)

    engine.execute(template) match {
      case Right(_) =>
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "run a table with multiple variables separated by rows" in {
    val text =
      """This is | a test.
        || head1 | head2 | head3 |
        || ----- | ----- | ----- |
        || [[var11]] | val12 | val13 |
        || val21 | val22 | val23 |
        || [[var31]] | val32 | val33 |
        || val41 | val42 | [[var43]] |
        |This is a test.""".stripMargin

    val compiledTemplate = compile(text)
    val parameters =
      TemplateParameters("var11" -> "hello", "var31" -> "world", "var43" -> "!")
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.variables.map(_.name.name) should contain allOf ("var11", "var31", "var43")
      case Left(ex) => fail(ex)
    }
  }

  it should "throw an error if a section name is used before it has been rendered" in {
    val text =
      """
        |^(section name) bla bla bla
        |
        |[[section name]]
        |
        |[[section name 2]]
        |
        |^(section name 2) bla bla bla
      """.stripMargin

    val compiledTemplate = compile(text)
    val parameters = TemplateParameters()
    engine.execute(compiledTemplate, parameters) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe """<p class="no-section"><br /></p><ul class="list-lvl-1"><li><p>1.  bla bla bla</p><p>1</p><p>2</p></li><li><p>2.  bla bla bla<br />      </p></li></ul>"""
      case Left(ex) => fail(ex)
    }
  }

  it should "not run that long" in {
    val text =
      """
        |####
        |show title:false;
        |####
        |<%
        |#set up formatting for the form
        |==Consignment Information==
        |[[Effective Date: Date]]
        |[[Consignor Name]]
        |[[Consignor Address: Address]]
        |[[Affiliate]]
        |[[Liability]]
        |[[Auction]]
        |[[Auction Location]]
        |[[Auction Month]]
        |[[Auction Year]]
        |[[Marketing Promises]]
        |[[Settlement Date]]
        |[[Review Rights]]
        |[[Hours to Review: Number]]
        |[[Commission]]
        |[[Percent Commission: Number]]
        |[[Expenses]]
        |[[Consignor Expenses]]
        |[[Percent of Hammer Price: Number]]
        |[[Reserves]]
        |[[Rescission]]
        |[[Representations and Warranty Provisions]]
        |[[Entity Type]]
        |[[Expense of Unsold Property Borne by]]
        |[[Lot Reconsignment]]
        |
        |==Signature Information==
        |[[Sothebys Signatory Email: Identity]]
        |[[Sothebys Signatory Name]]
        |[[Sothebys Signatory Title]]
        |[[Entity Name]]
        |[[Consignor Signatory Email: Identity]]
        |
        |==Schedule I==
        |[[Properties:Collection<PropertiesInfo> "List of Properties"]]
        |
        |==Other Schedule Information==
        |[[Consignor Account Number: Number]]
        |[[Payee Account Name]]
        |[[Bank Name]]
        |[[Bank Address: Address]]
        |[[Bank Account Number]]
        |[[Sort or Swift Code or ABA Number]]
        |[[IBAN Number]]
        |[[Intermediary Bank Name]]
        |[[Intermediary Bank Address: Address]]
        |
        |#set choices
        |[[Location: Choice("Geneva", "Doha", "Dubai", "Hong Kong", "London", "Milan", "New York", "Paris", "Zurich")]]
        |[[Auction Location: Location]]
        |[[Reps: Choice("Agent", "Estate", "Entity", "Trust")]]
        |[[Representations and Warranty Provisions: Reps]]
        |[[Unsold: Choice("Consignor", "Sotheby's")]]
        |[[Expense of Unsold Property Borne by: Unsold]]
        |[[Lots: Choice("Sold in London and Under TA", "Property in Free Circulation", "To Paris by Individual", "To Paris by Corporation", "To London and Paris by Individual", "To London and Paris by Corporation", "To Hong Kong", "To Another Sotheby's Location")]]
        |[[Lot Reconsignment: Lots]]
        |[[EntityTypes: Choice("corporation","limited liability company","limited partnership","an entity")]]
        |[[Entity Type: EntityTypes]]
        |[[ReviewTypes: Choice("your review and commentary", "your review, comment and approval")]]
        |[[ReviewRights: ReviewTypes]]
        |
        |%>
        |
        |\centered **CONSIGNMENT AGREEMENT**
        |
        |[[Effective Date: Date]]
        |[[Consignor Name]]
        |[[Consignor Address: Address]]
        |
        |Thank you for consigning property to Sotheby’s.  This confirms our agreement ("Agreement") under which property which is consigned by you and is listed on Schedule I hereto (the "Property") will be offered by us for sale at auction, subject to the following terms and our standard Conditions of Sale and Terms of Guarantee to be printed in the catalogue for the sale, by which you hereby agree to be bound.  As used herein, "we", "us", "our" and "Sotheby’s" mean Sotheby’s, Inc. {{Affiliate "Are there any affiliated Sotheby's company offering Property for sale under this Agreement?" => and any affiliated company offering Property for sale under this Agreement}}, and "you" and "your" mean [[Consignor Name]]{{Liability "Is the Consignor jointly and severally liable?" =>, jointly and severally}}.
        |
        |^**The Auction.**  The Property will be offered for sale in our [[Auction Location]] auction location, on [[Auction Month]] [[Auction Year]], subject to postponement for reasons beyond our control. In connection with {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} auction, we will have absolute discretion as to (a) consulting any expert either before or after the sale, (b) researching the provenance of the Property either before or after the sale, (c) {{!Auction "Will the Property be offered at a specific auction?" => grouping the Property into lots and}} providing catalogue and other descriptions as we believe appropriate, (d) the {{Auction "Will the Property be offered at a specific auction?" => date}}{{!Auction "Will the Property be offered at a specific auction?" => dates}} of an auction, (e) the marketing and promotion of the sale and (f) the manner of conducting the sale.
        |
        |{{Marketing Promises "Include Marketing Promises Insert?" => We agree to market and promote the Property as set forth on Schedule II hereto subject to your timely execution of this Agreement, our timely receipt of the Property and our obtaining the appropriate copyright clearances within the applicable print publication deadlines.}}
        |
        |{{Review Rights "Include Catalogue Review Rights Insert?" => Notwithstanding the foregoing, we will provide you with the portion of the catalogue drafts and other descriptions pertaining to the Property, as we believe appropriate, which may appear in any marketing or sales materials.  You will have at least [[Hours to Review]] hours to review any such text and we will reasonably incorporate your comments and suggestions; provided, that, ultimate editorial control over any such catalogues, text or materials will be maintained by Sotheby’s.}}
        |
        |^**Commission.**  {{Commission "Is the Consignor paying a sales commission?" => You will pay us a selling commission equal to [[Percent Commission | raw]]% of the hammer price of each lot of Property sold.}}{{!Commission "Is the consignor paying a sales commission?" => You will not pay us any selling commission on the Property.}}  You authorize us to charge the buyer and retain for our account a commission on {{Auction "Will the Property be offered at a specific auction?" => the Property}}{{!Auction "Will the Property be offered at a specific auction?" => each lot sold}} (the "buyer’s premium").  The Conditions of Sale in the catalogue for the auction will state the rate at which the buyer’s premium will be assessed against the buyer, and such rate will be a percentage of the hammer price of {{Auction "Will the Property be offered at a specific auction?" => the Property}}{{!Auction "Will the Property be offered at a specific auction?" => each lot sold}}.
        |
        |^**Settlement.**
        |
        |^^On the {{Settlement Date "Is the settlement occurring after a specific auction?" => Settlement Date}}{{!Settlement Date "Is the settlement occurring after a specific auction?" => Settlement Dates}} (as defined below), we will mail to you or wire transfer to you pursuant to your Payment Instructions (as defined below) the sale proceeds we collect and receive, after deducting our buyer’s premium (the "net sale proceeds"), unless the purchaser has notified us of its intention to rescind the sale (as provided in paragraph 9).  We may also deduct and retain from the net sale proceeds any other amount you owe us or any of our affiliated entities, whether arising out of the sale of the Property or otherwise. {{Settlement Date "Is the settlement occurring after a specific one auction?" => The "Settlement Date" will be the date that is thirty-five days after the last session of the auction.}}{{!Settlement Date "Is the settlement occurring after a specific one auction?" => The "Settlement Dates" will be the dates that are thirty-five days after the last session of each auction.}}
        |
        |^^We have no obligation to enforce payment by any purchaser.  If a purchaser does not pay, and you and we do not agree on another course of action, we reserve the right to cancel the sale and return the Property to you.  Notwithstanding the preceding sentence, if we pay you any portion of the net sale proceeds for {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property and have not collected from the purchaser of {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => such}} Property payment of the amount we paid to you, simultaneously with any such payment by us to you, you assign to us any and all rights you may have against such purchaser to the extent of such payment, whether at law, in equity or under the Conditions of Sale.  You agree to execute any documents we may reasonably request evidencing this assignment and you agree that all of your representations, warranties and indemnities set forth in this Agreement shall apply to us or the purchaser, as the case may be, with respect to the Property.  You authorize us, in our sole discretion, to impose on {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} purchaser and retain for our account a late charge if payment is not made in accordance with the Conditions of Sale.
        |
        |^^You agree that we may offer the purchaser of any lot of the Property the right to pay the total purchase price for such Property (including buyer’s premium) in one or more installments as determined by us in our sole discretion over a period of up to 90 days following the auction.  The purchaser will not pay any interest on any installment.   We agree to notify you following the auction if extended payment terms have been offered to and accepted by any purchaser, the dates on which installments of the net sale proceeds are due to you (each, an "Installment Payment Date") and the amount which is due to you on each Installment Payment Date.
        |
        |^^Following our receipt of (i) your signed instructions or (ii) in the event we do not receive your signed instructions in connection with this Agreement, the payment instructions we receive in a manner that is mutually agreed upon or in a manner that is a customary form of communication between you and us (collectively, the "Payment Instructions"), we are hereby authorized to make a payment pursuant to the Payment Instructions and we shall have no liability for any loss, claim, or damage you sustain as a result of our reliance upon such Payment Instructions regardless of whether such Payment Instructions resulted from any unauthorized or fraudulent activity by a third party.
        |
        |^**Reserves.**
        |
        |^^{{Auction "Will the Property be offered at a specific auction?" => The}}{{!Auction "Will the Property be offered at a specific auction?" => Each lot of the}} Property will be offered subject to {{Reserves "Will the Reserves be mutually agreed to prior to the date of the sale?" => a reserve to be mutually agreed upon prior to the date of sale.}}{{!Reserves "Will the Reserves be mutually agreed to prior to the date of the sale?" => the reserve set forth on Schedule I hereto, unless we mutually agree upon a different reserve prior to the date of sale.}}  However, we may sell {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any lot of the}} Property at a price below the reserve, provided that we pay you in accordance with paragraph 3 above {{Reserves "If Property sold below reserve, consignor is entitled to agreed reserve?" => the agreed reserve}}{{!Reserves "If Property sold below reserve, consignor is entitled to agreed reserve?" => the net amount which you would have been entitled to received had the Property been sold at the agreed reserve.}}  No reserve for a lot will exceed its low presale estimate.
        |
        |^^You agree not to bid on the Property.  Although we shall be entitled to bid on your behalf up to the amount of the reserve, you shall not instruct or permit any other person to bid for the Property on your behalf.  If, however, you violate your foregoing commitment and you or your agent becomes the successful bidder on the Property, you will pay us the buyer’s premium on the hammer price, the Property may be sold without any reserve, and you will not be entitled to the benefit of any warranties under the Conditions of Sale or Terms of Guarantee.
        |
        |^^There will be no commission if the Property is bought-in for failing to reach its reserve.  In the event {{Auction "Will the Property be offered at a specific auction?" => the Property}}{{!Auction "Will the Property be offered at a specific auction?" => any lot}} is bought-in, we will announce that such lot has been "passed", "withdrawn", "returned to owner", or "bought-in".
        |
        |^**Representations and Warranties; Indemnity.**
        |
        |You represent and warrant to us and {{Auction "Will the Property be offered at a specific auction?" => the purchaser}}{{!Auction "Will the Property be offered at a specific auction?" => each purchaser}} that: {{Representations and Warranty Provisions = "Agent" => You represent and warrant to us and the purchaser that:...you are the sole and absolute owner of the Property, provided that, to the extent you are acting as an agent for someone who is not signing this Agreement, you instead represent and warrant that your principal is the sole and absolute owner of the Property, and your principal has expressly authorized you to engage additional agents/brokers such as Sotheby’s to act on its behalf in selling the Property, and your principal has expressly authorized the commissions payable to Sotheby’s on the sale of the Property; you have the right to consign the Property for sale;}} you have the right to consign the Property for sale;  it is now, and through and including its sale will be kept, free of all liens, claims and encumbrances of others, including, but not limited to, claims of governments or governmental agencies; {{Representations and Warranty Provisions = "Estate" => except for federal and state estate tax liens (which you represent and warrant will be adequately provided for and will be paid when due); good title and right to possession will pass to the purchaser free of all liens, claims and encumbrances;}} good title to and right to possession of the Property will pass to the purchaser free of all liens, claims and encumbrances; this Agreement has been duly authorized, executed and delivered by you and constitutes your legally binding obligation; {{Representations and Warranty Provisions = "Entity" => you are a  [[Entity Type]] duly organized, validly existing and in good standing in the jurisdictions where such qualification is required and have full {{Entity Type = "corporation" => corporate}} power and authority to execute, deliver and perform your obligations under this Agreement;}}{{Representations and Warranty Provisions = "Estate" => the signatories to this Agreement for the Estate have been duly appointed and are validly serving as executors of the Estate and have all requisite power and authority under the documents establishing the Estate and law to execute and deliver this Agreement; the signatories to this Agreement for the Estate are all the executors of the Estate;}}{{Representations and Warranty Provisions = "Trust" => the signatories to this Agreement for the Trust have been duly appointed and are validly serving as trustees of the Trust and have all requisite power and authority under the trust agreement and law to execute and deliver this Agreement; the signatories to this Agreement for the Trust are all the trustees of the Trust;}} you have provided us with all information you have concerning the provenance, condition and restoration of the Property; you have no reason to believe that {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any lot of}} Property is not authentic or is counterfeit; where the Property has been imported into the United States, the Property has been lawfully imported into the United States and has been lawfully and permanently exported as required by the laws of any country (including any laws or regulations applicable in the European Union) in which it was located; required declarations upon the export and import of the Property have been properly made and any duties and taxes on the export and import of the Property have been paid; you have paid or will pay any and all taxes and/or duties that may be due on the net sale proceeds of the Property and you have notified us in writing of any and all taxes and/or duties that are payable by us on your behalf in any country other than the United States; and there are no restrictions on our right to reproduce photographs of the Property.  We retain the exclusive copyright to all catalogue and other illustrations and descriptions of the Property created by us.
        |
        |^^You agree to indemnify and hold us and each purchaser harmless from and against any and all claims, actions, damages, losses, liabilities and expenses (including reasonable attorneys’ fees) relating to the breach or alleged breach of any of your agreements, representations or warranties in this Agreement.
        |
        |^^Your representations, warranties and indemnity will survive completion of the transactions contemplated by this Agreement.
        |
        |^**Expenses.**  {{Consignor Expenses "Is the Consignor bearing the expenses related to the sale of the Property?" => You agree to pay:
        |^^a property management fee of: (i) for items of Property which have been sold, [[Percent of Hammer Price | raw]]% of the hammer price (excluding buyer’s premium), or for items of Property not yet offered for sale, [[Percent of Hammer Price | raw]]% of the mean of our latest pre-sale estimates, or (ii) for items of Property which have failed to sell at auction, [[Percent of Hammer Price | raw]]% of the reserve, in each case except for those items of Property which have specific terms noted on the attached Property Schedule. Such fee covers property handling, property administration, and bearing liability for loss or damage to Property in our possession;
        |^^our standard fees then in effect for catalogue illustration;
        |^^ packing, shipping and customs duties to our premises;
        |^^the cost of any agreed-upon advertising; and
        |^^the cost of other services, such as framing, restoration and gemological tests, approved by you.
        |In addition to other remedies available to us by law, we reserve the right to impose and retain a late charge of 1.5% per month on any amount due us or any of our affiliated entities and remaining unpaid for more than fifteen days after we notify you.}}{{Expenses "Is Sotheby's bearing the expenses related to the sale of the Property?" => We agree to bear all expenses related to the sale of the Property, including but not limited to:  (a) the cost of bearing liability for any loss or damage to the Property as provided in paragraph 7 hereof, (b) catalogue illustration, production and mailing, (c) packing and shipping to our premises and (d) any agreed-upon advertising.}}
        |
        |^**Loss or Damage to Property.** We agree to bear liability for any loss or damage to the Property from the time we receive the Property and while the Property is in our custody and control. We will pay you the Value of the Property (as defined below) or the Depreciation Amount (as defined below), as the case may be, in the event of loss or damage as set forth below.  The maximum amount of our liability to you resulting from loss or damage to the Property shall not exceed the Value of the Property for such Property.  For purposes of this limitation of liability, the Value of the Property is:  (a) for Property which has been sold, the hammer price (excluding buyer’s premium), (b) for Property which has failed to sell at auction, the reserve, or (c) for Property not yet offered for sale, the mean of our latest presale estimates.  In the event of a total loss (Property which has been lost, or Property which has been damaged and has depreciated in value, in our opinion, by 50% or more), we will pay you the Value of the Property for such Property, and simultaneously with such payment, all title and interest to the Property shall pass to us.  In the event of a partial loss (Property which has been partially damaged or lost and has depreciated in value, in our opinion, by less than 50%), we will pay you the amount of depreciation, as determined by us in our discretion (the "Depreciation Amount"), and such Property will be offered for sale or, at your request, returned to you.  We will not be responsible for Property that is not within our custody and control or liable for damage to frames or glass covering prints, paintings or other works, for damage occurring in the course of any process undertaken by independent contractors employed with your consent (including restoration, framing or cleaning), or for damage caused by changes in humidity or temperature, inherent conditions or defects, normal wear and tear, war, acts of terrorism, nuclear fission or radioactive contamination, or chemical, bio-chemical or electromagnetic weapons. We maintain insurance for loss or damage to all property that is under our custody and control. In the event of any loss of or damage to the Property as described in this paragraph 7, you agree that your sole remedy against us will be the payment of the Value of the Property or the Depreciation Amount (the "Payment"), as the case may be, and upon receipt of the Payment by you, you irrevocably release and discharge Sotheby’s, on your own behalf and on behalf of any insurer you may have, from all liability for loss or damage to the Property resulting from any cause whatsoever, including but not limited to the negligence of Sotheby’s and its agents and independent contractors.
        |
        |^**Withdrawal.**  You may not withdraw {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property from sale after the date on which you sign this Agreement.  Regardless of whether we have previously issued a receipt for the Property, published a catalogue including the Property or advertised its sale, we may withdraw {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property at any time before sale if in our sole judgment (a) there is doubt as to its authenticity or attribution or its sale would or may subject us and/or you to any liability, (b) there is doubt as to the accuracy of any of your representations or warranties, (c) you have breached any provision of this Agreement, (d) {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => lot of}} Property incurs loss or damage so that it is not in the state in which it was when we agreed to offer it for sale, or (e) we determine in our reasonable  discretion that its sale may be detrimental to our reputation and/or brand.  If we withdraw {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property under (b) or (c) of this paragraph 8, you must within ten days of our notice to you of withdrawal pay us a withdrawal fee equal to 20% of the mean of our latest presale estimates for the withdrawn Property, as well as all out-of-pocket expenses incurred by us up to and including the date of withdrawal (the "Withdrawal Fee").  If {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property is withdrawn by you in breach of this Agreement, you will pay us a Withdrawal Fee as well as any special, incidental or consequential damages incurred as a result of your breach, notwithstanding anything to the contrary in this Agreement.  If {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property is withdrawn under (a) or (d) above, you will not be charged a Withdrawal Fee.  If {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property is withdrawn from sale under (e) above you will not be charged a Withdrawal Fee unless you failed to disclose to us prior to the sale any facts or circumstances known to you which are relevant for the purpose of our determination under (e). Subject to any liens against or claims to the Property, the withdrawn Property will be returned to you at our expense promptly following your payment of the Withdrawal Fee, if applicable. If {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property is withdrawn for any reason, the timing and the content of any announcement regarding the withdrawal shall be in Sotheby’s sole discretion.
        |
        |^**Rescission.**  You authorize us to rescind the sale of {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property in accordance with the Conditions of Sale and Terms of Guarantee, or if we learn that the Property is inaccurately described in the catalogue, or if we learn that the Property is a counterfeit (a modern forgery intended to deceive) or if we determine in our sole judgment that the offering for sale of {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any lot of}} Property has subjected or may subject us and/or you to any liability, including liability under the warranty of title or warranty of authenticity included in the Terms of Guarantee.  If we receive from a purchaser notice of intention to rescind and we determine that {{!Auction "Will the Property be offered at a specific auction?" => a lot of}} the Property is subject to rescission under the Terms of Guarantee or as otherwise set forth above, we will credit the purchaser with the purchase price, you will return to us any sale proceeds {{Rescission "Is there a fee associated with the return of Property?" => and any fee}} for such Property paid by us to you or to a third party as directed by you, and you will reimburse us for expenses incurred in connection with the rescinded sale, including the reasonable attorney’s fees we incur in collecting from you any amounts due herein, and pay us any other amounts you owe us or any of our affiliated entities.  Upon such reimbursement and payment, we will release the Property to you, except in the event of an adverse claim of title, in which case we will retain the Property until such claim has been resolved.
        |
        |^**Private Sales.**  If {{Auction "Will the Property be offered at a specific auction?" => the Property}}{{!Auction "Will the Property be offered at a specific auction?" => any lot}} fails to reach its reserve and is bought-in for your account, you authorize us, as your exclusive agent, for a period of 60 days following the auction, to sell the lot privately for a price that will result in a payment to you of not less than the {{Reserves "If Property sold below reserve, consignor is entitled to agreed reserve?" => the agreed reserve}}{{!Reserves "If Property sold below reserve, consignor is entitled to agreed reserve?" => the net amount which you would have been entitled to received had the Property been sold at the agreed reserve.}}  In such event, your obligations to us hereunder with respect to such lot are the same as if it had been sold at auction.
        |
        |^**Treatment of Unsold Property.**  If {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property remains unsold for any reason after the auction, we will notify you.  If {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => such}} Property has not been sold privately pursuant to paragraph 10, and if it is not reconsigned to us for sale on mutually agreed-upon terms or picked up within 60 days after such notification, we will return it to you at {{Expense of Unsold Property Borne by = "Consignor" => your}}{{Expense of Unsold Property Borne by = "Sotheby's" => our}} expense.
        |
        |^**Estimates; Catalogue Descriptions.**
        |
        |^^Presale estimates, if any, are intended as guides for prospective bidders.  We make no representation or warranty of the anticipated selling price of {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property and no estimate anywhere by us of the selling price of {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property may be relied upon as a prediction of the actual selling price.  Estimates included in receipts, catalogues or elsewhere are preliminary only and are subject to revision by us from time to time in our sole discretion.
        |
        |^^We will not be liable for any errors or omissions in the catalogue or other descriptions of the Property and make no guarantees, representations or warranties whatsoever to you with respect to the Property, its authenticity, attribution, legal title, condition, value or otherwise.
        |
        |^**Use of Name.**  We may designate the Property when we offer it for sale, advertise or otherwise promote the sale, both before or after the auction, as you and we may mutually agree.
        |
        |^**Legal Status.**
        |
        |^^If you are acting as a fiduciary in executing this Agreement and in the transactions contemplated hereunder, please sign and return to us our standard "Fiduciary Agreement".
        |
        |^^If you are acting as an agent for someone who is not signing this Agreement, you and your principal jointly and severally assume your obligations and liabilities hereunder to the same extent as if you were acting as principal.
        |
        |^**Reconsignment.**
        |
        |^^We may, at our discretion, reconsign {{Auction "Will the Property be offered at a specific auction?" => the}}{{!Auction "Will the Property be offered at a specific auction?" => any}} Property so that it shall be offered for sale at public auction by one of our affiliates, unless you object in writing within ten days of the date of our notice of reconsignment. {{Auction "Will the Property be offered at a specific auction?" => The reconsigned Property}}{{!Auction "Will the Property be offered at a specific auction?" => Any reconsigned Property}} shall be offered for sale pursuant to the terms of this Agreement, and subject to the Conditions of Business and the Authenticity Guarantee, if any, applicable to the auction where offered.  If there is a conflict between the applicable Conditions of Business and the Authenticity Guarantee and the terms of this Agreement, the terms of this Agreement shall control.  With respect to any such reconsigned lot, the terms "Conditions of Sale" and "Terms of Guarantee" referred to in this Agreement shall mean the Conditions of Business and the Authenticity Guarantee, if any, applicable to such auction.  {{Auction "Will the Property be offered at a specific auction?" => The}}{{!Auction "Will the Property be offered at a specific auction?" => Any}} net sale proceeds of the Property in such sale shall be remitted to you in the currency in which the auction is conducted, and all local taxes shall apply.
        |
        |{{Lot Reconsignment = "Sold in London and Under TA" => We will reconsign any lot of Property designated on Schedule I hereto for sale in London so that it shall be offered for sale at public auction by our affiliate in London, Sotheby’s ("Sotheby’s London"). You acknowledge that the Property has been or will be imported from outside the European Union for sale in London and that Sotheby's London will be selling the Property under its Temporary Admission arrangement. Sotheby’s London will charge the buyer UK Import VAT at the prevailing rate on the hammer price and UK domestic VAT at the standard rate on the buyer's premium for the Property.  You acknowledge that any applicable Artist’s Resale Right royalty due on the sale of the Property will be charged to the buyer.}}{{Lot Reconsignment = "Property in Free Circulation" => ^^You have confirmed to us that the items of Property which will be offered for sale by Sotheby’s London (individually and collectively, the "London Property") are in free circulation within the European Union and you are neither registered for VAT in the European Union nor are you under a legal obligation to register for VAT in the European Union. You undertake to inform Sotheby’s immediately if, after the signature of this Agreement by you, you register for VAT or become under a legal obligation to register for VAT in the European Union. Sotheby’s London will charge the buyers of the London Property, the applicable import VAT at the prevailing rates on the hammer price and the applicable domestic VAT at the standard rate on the buyer’s premium for such items of Property.}}
        |
        |{{Lot Reconsignment = "To Paris by Individual" => We will reconsign any lot of Property designated on Schedule I hereto for sale in Paris (the "Paris Property") so that it shall be offered for sale at public auction by our affiliate in Paris, Sotheby’s France ("Sotheby’s France").   The Paris Property shall be offered for sale pursuant to the terms of this Agreement, and subject to the Conditions of Business and the Authenticity Guarantee, applicable to the auction in which the Paris Property is offered.  If there is a conflict between the applicable Conditions of Business and the Authenticity Guarantee and the terms of this Agreement, the terms of this Agreement shall control.  With respect to any such lot of Paris Property, the terms "Conditions of Sale" and "Terms of Guarantee" referred to in this Agreement shall mean the Conditions of Business and the Authenticity Guarantee applicable to the auction. Any net sale proceeds collected and received for the Paris Property shall be remitted to you in the currency in which the applicable auction is conducted, and all local taxes and regulatory charges shall apply.
        |
        |You acknowledge and agree that the Paris Property may be imported from outside the European Union for sale in the European Union, and Sotheby’s France, as applicable, will be selling the Paris Property under their respective Temporary Admission arrangements. Sotheby’s France will charge the buyers of the Paris Property the applicable import VAT at the prevailing rates on the hammer price and the applicable domestic VAT at the standard rate on the buyer’s premium for such items of Property.
        |
        |You hereby further warrant to Sotheby’s that neither you nor any of your beneficiaries are a French resident for tax purposes.  Please note that Sotheby’s will not deduct Taxe Forfaitaire from the hammer price for each lot of Property sold on the condition that you provide Sotheby’s with a declaration that neither you nor any of your beneficiaries are a French resident for tax purposes as well as a copy of your proof of identity and proof of the identity of your beneficiaries and any additional documents required by the French Tax Administration or the French Customs in connection thereto. If these conditions are not fulfilled within five days of the date of the sale of the Property, you will be liable to Sotheby’s for the amount of the Taxe Forfaitaire due as well as any related amounts.
        |
        |To the extent that any Artist Resale Right ("Droit de Suite") is applicable on the sale of the Paris Property Sotheby’s Paris will deduct an amount equal to the droit de suite at the applicable rate from the hammer price for each lot of the Paris Property, where applicable.}}
        |
        |{{Lot Reconsignment = "To Paris by Corporation" => We will reconsign any lot of Property designated on Schedule I hereto for sale in Paris so that it shall be offered for sale at public auction by our affiliate in France, Sotheby’s France (individually and collectively, the "Paris Property"). You acknowledge that the Paris Property may be imported from outside the European Union for sale in the European Union, and Sotheby’s France (if applicable) will be selling such items of Property under their Temporary Admission arrangements. Sotheby’s France will charge the buyers of any such items of Paris Property the applicable import VAT at the prevailing rates on the hammer price and the applicable domestic VAT at the standard rate on the buyer’s premium for such items of Paris Property.
        |
        |With respect to the Paris Property, you hereby represent to Sotheby’s that you are a professional liable, as a result of the sale, to corporation tax or income tax according to the applicable regulations relating to business profits under common law, and that each Property sold is included in the assets of your balance sheet. Please note that Sotheby’s rely on your representations in order not to deduct Taxe Forfaitaire from the hammer price for each lot of Property sold. You undertake to provide Sotheby’s with evidence of your company’s registration and any additional documents required by the French Tax Administration or the French Customs, as the case may be, in connection thereto.  In the event that your representations are incorrect, you will be liable to Sotheby’s for the amount of the Taxe Forfaitaire due as well as any related amounts.
        |
        |To the extent that any Artist Resale Right ("Droit de Suite") is applicable on the sale of any of the Paris Property, Sotheby’s France will deduct an amount equal to the Droit de Suite at the applicable rate from the hammer price for each lot of the Paris Property, where applicable.}}
        |
        |{{Lot Reconsignment = "To London and Paris by Individual" => We will reconsign any lot of Property designated on Schedule I hereto (i) for sale in London so that it shall be offered for sale at public auction by our affiliate in London, Sotheby’s ("Sotheby’s London") and (ii) for sale in Paris so that it shall be offered for sale at public auction by our affiliate in Paris, Sotheby’s France ("Sotheby’s France").   Any reconsigned lot (individually and collectively, the "Reconsigned Property") shall be offered for sale pursuant to the terms of this Agreement, and subject to the Conditions of Business and the Authenticity Guarantee, if any, applicable to the auction where the Reconsigned Property is offered.  If there is a conflict between the applicable Conditions of Business and the Authenticity Guarantee and the terms of this Agreement, the terms of this Agreement shall control, except with respect to the applicable law and jurisdictions which shall be those applicable to the sale in London or in France, as the case may be.  With respect to any such  lot of Reconsigned Property, the terms "Conditions of Sale" and "Terms of Guarantee" referred to in this Agreement shall mean the Conditions of Business and the Authenticity Guarantee, if any, applicable to the auction in the relevant selling location. Any net sale proceeds collected and received for the Reconsigned Property shall be remitted to you in the currency in which the applicable auction is conducted, and all local taxes and regulatory charges shall apply.
        |
        |You acknowledge and agree that the items of Reconsigned Property which will be offered for sale by Sotheby’s  London (individually and collectively, the "London Property") and the items of Reconsigned Property which will be offered for sale by Sotheby’s France (individually and collectively, the "Paris Property"), may be imported from outside the European Union for sale in the European Union, and Sotheby’s London and Sotheby’s France, as applicable, will be selling the London Property and the Paris Property, respectively, under their respective Temporary Admission arrangements. Sotheby’s London and Sotheby’s France will charge the buyers of the London Property and the Paris Property, respectively, the applicable import VAT at the prevailing rates on the hammer price and the applicable domestic VAT at the standard rate on the buyer’s premium for such items of Property.
        |
        |You hereby further warrant to Sotheby’s that you are not a French resident for tax purposes.  Please note that Sotheby’s will not deduct Taxe Forfaitaire from the hammer price for each lot of Paris Property sold on the condition that you provide Sotheby’s with a declaration that you are not a French resident for tax purposes as well as a copy of your proof of identity and any additional documents required in connection thereto. If these conditions are not fulfilled within five (5) days of the date of the sale of the Paris Property, you will be liable to Sotheby’s for the amount of the Taxe Forfaitaire due as well as any related amounts.
        |
        |To the extent that any Artist Resale Right ("Droit de Suite") is applicable on the sale of any of the Reconsigned Property (i) Sotheby’s Paris will deduct an amount equal to the droit de suite at the applicable rate from the hammer price for each lot of the Paris Property, where applicable, and Sotheby’s London will charge the buyer any Artist’s Resale Right levy payable on the sale of the London Property, where applicable.}}
        |
        |{{Lot Reconsignment = "To Paris by Corporation" => We will reconsign any lot of Property designated on Schedule I hereto (i) for sale in London so that it shall be offered for sale at public auction by our affiliate in London, Sotheby’s ("Sotheby’s London") and (ii) for sale in Paris so that it shall be offered for sale at public auction by our affiliate in Paris, Sotheby’s France ("Sotheby’s France").   Any reconsigned lot (individually and collectively, the "Reconsigned Property") shall be offered for sale pursuant to the terms of this Agreement, and subject to the Conditions of Business and the Authenticity Guarantee, if any, applicable to the auction where the Reconsigned Property is offered.  If there is a conflict between the applicable Conditions of Business and the Authenticity Guarantee and the terms of this Agreement, the terms of this Agreement shall control, except with respect to the applicable law and jurisdictions which shall be those applicable to the sale in London or in France, as the case may be.  With respect to any such  lot of Reconsigned Property, the terms "Conditions of Sale" and "Terms of Guarantee" referred to in this Agreement shall mean the Conditions of Business and the Authenticity Guarantee, if any, applicable to the auction in the relevant selling location. Any net sale proceeds collected and received for the Reconsigned Property shall be remitted to you in the currency in which the applicable auction is conducted, and all local taxes and regulatory charges shall apply.
        |
        |You acknowledge and agree that the items of Reconsigned Property which will be offered for sale by Sotheby’s  London (individually and collectively, the "London Property") and the items of Reconsigned Property which will be offered for sale by Sotheby’s France (individually and collectively, the "Paris Property"), may be imported from outside the European Union for sale in the European Union, and Sotheby’s London and Sotheby’s France, as applicable, will be selling the London Property and the Paris Property, respectively, under their respective Temporary Admission arrangements. Sotheby’s London and Sotheby’s France will charge the buyers of the London Property and the Paris Property, respectively, the applicable import VAT at the prevailing rates on the hammer price and the applicable domestic VAT at the standard rate on the buyer’s premium for such items of Property.
        |
        |With respect to the Paris Property, you hereby represent to us that you are a professional liable, as a result of the sale, for corporation tax or income tax according to the applicable regulations relating to business profits under common law, and that each item of Paris Property sold is included in the assets of your balance sheet. Sotheby’s will rely on your representation in order not to deduct Taxe Forfaitaire from the hammer price for each lot of Paris Property sold. You will provide Sotheby’s with evidence of your company’s registration and any additional documents required by the French Tax Administration or the French Customs, as the case may be, in connection thereto.  In the event that your representations are incorrect, you will be liable to Sotheby’s for the amount of the Taxe Forfaitaire due as well as any related amounts.
        |
        |To the extent that any Artist Resale Right ("droit de suite") is applicable on the sale of any of the Paris Property, Sotheby’s Paris will deduct an amount equal to the droit de suite at the applicable rate from the hammer price for each lot of the Paris Property, where applicable, and Sotheby’s London will charge the buyer any Artist’s Resale Right levy payable on the sale of the London Property, where applicable.}}
        |
        |{{Lot Reconsignment = "To Hong Kong" => We will reconsign any lot of Property designated on Schedule I hereto for sale in Hong Kong so that it shall be offered for sale at public auction by our affiliate Sotheby’s Hong Kong Ltd.}}
        |
        |{{Lot Reconsignment = "To Another Sotheby's Location" => Any reconsigned lot shall be offered for sale pursuant to the terms of this Agreement, and subject to the Conditions of Business and the Authenticity Guarantee, if any, applicable to the auction where offered.  If there is a conflict between the applicable Conditions of Business and the Authenticity Guarantee and the terms of this Agreement, the terms of this Agreement shall control.  With respect to any such reconsigned lot, the terms "Conditions of Sale" and "Terms of Guarantee" referred to in this Agreement shall mean the Conditions of Business and the Authenticity Guarantee, if any, applicable to such auction. Any net sale proceeds of the Property in such sale shall be remitted to you in the currency in which the auction is conducted, and all local taxes shall apply.}}
        |
        |^**Amendment.**  Neither you nor we may amend, supplement or waive any provision of this Agreement other than by means of a writing signed by both parties.
        |
        |^**Privacy.**  Sotheby’s, its subsidiaries and affiliates ("Sotheby’s Group") will record any information that you supply to us or that we obtain about you in its data systems shared within Sotheby’s Group.  Your information will be kept confidential within Sotheby’s Group.  From time to time Sotheby’s Group may send you information about its sales and events, or about products and services of other organizations with which it has a relationship.
        |
        |^**No Legal or Tax Advice.**  This Agreement is an important legal document. You acknowledge that you have had the opportunity to consult an attorney before signing this Agreement and have signed this Agreement after having the opportunity to consult with an attorney of your own choosing. Notwithstanding any references to any transactions or arrangements in this Agreement, or any contemporaneous written, oral or implied understandings of the parties relating to the subject matter of this Agreement, Sotheby’s has not provided legal or tax advice or tax planning to you or for your benefit in connection with the transactions contemplated by this Agreement, and no one at Sotheby’s has acted as your attorney or tax advisor.  You have carefully read this Agreement in its entirety, understand all of its terms, and knowingly and voluntarily agree to all of the terms and conditions contained herein.
        |
        |^**Miscellaneous.**  This Agreement shall be governed by and construed and enforced in accordance with the laws of the State of New York.  In the event of a dispute hereunder, you agree to submit to the exclusive jurisdiction of the state courts of and the federal courts sitting in the State and County of New York.  This Agreement shall be binding upon your heirs, executors, beneficiaries, successors and assigns, but you may not assign this Agreement without our prior written consent.  Neither party shall be liable to the other for any special, consequential, incidental or punitive damages.  This Agreement, including the Schedules hereto, and the Conditions of Sale and any Terms of Guarantee, constitute the entire agreement between the parties with respect to the transactions contemplated hereby and supersede all prior or contemporaneous written, oral or implied understandings, representations and agreements of the parties relating to the subject matter of this Agreement. You agree that you will not disclose the terms of this Agreement to any third party without our prior written consent, except to attorneys and accountants on a need-to-know basis, or as a result of valid legal process compelling the disclosure, provided you first give us prompt written notice of such service of process and allow us, if we deem it appropriate, to obtain a protective order.  You agree to furnish us, upon our request, with any additional information required to comply with applicable law. Any notices given hereunder to you or us shall be in writing to the respective addresses indicated on the first page of this Agreement (or to such other address as you or we may notify the other in writing) and shall be deemed to have been given five calendar days after mailing to such address or one business day after delivery by hand or telecopier. You agree to provide us, upon our request, verification of identity in an appropriate form. In the event we receive a subpoena from you or a third party relating to the Property or the Agreement, you shall pay us the costs we incur, including reasonable attorney’s fees, in responding to the subpoena. The paragraph headings contained in this Agreement are for convenience of reference only and shall not affect in any way the meaning or interpretation of this Agreement.  This Agreement may be executed in counterparts, each of which will be deemed an original and all of which together constitute one and the same instrument. Signatures sent by facsimile transmission and scanned executed agreements in PDF format sent by email transmission are each valid and binding and will be deemed an original.
        |
        |\pagebreak
        |
        |Please confirm your agreement with the foregoing by dating, signing and returning to us the duplicate copy of this Agreement.
        |
        |Very truly yours,
        |
        |**SOTHEBY’S, INC.**
        |
        |[[Sothebys Signatory Email: Identity | Signature]]
        |_______________________________
        |By: [[Sothebys Signatory Name]]
        |Title: [[Sothebys Signatory Title]]
        |
        |
        |**ACCEPTED AND AGREED:**
        |
        |**[[Entity Name | Uppercase]]**
        |
        |[[Consignor Signatory Email: Identity | Signature]]
        |_______________________________
        |By: [[Consignor Name]]
        |Dated: [[Effective Date: Date]]
        |
        |Sotheby’s, Inc.
        |License No. 1216058
        |
        |\pagebreak
        |
        |
        |\centered **SCHEDULE I**
        |\centered **Property Estimates**
        |<%
        |[[PropertiesInfo:Structure(
        |Property Name:Text;
        |Estimate:Text)]]
        |
        |[[Properties:Collection<PropertiesInfo> "List of Properties"]]
        |%>
        |
        |{{#for each Property:Properties =>
        |[[Property.Property Name]] [[Property.Estimate]]
        |}}
        |
        |{{Marketing Promises =>
        |\pagebreak
        |\centered **SCHEDULE II**
        |
        |(insert Schedule II)
        |}}
        |\pagebreak
        |
        |[[Consignor Name]]
        |[[Consignor Account Number: Number | raw]]
        |[[Consignor Address: Address]]
        |[[Effective Date: Date]]
        |
        |\centered **Payment Instructions**
        |
        |\centered If you require payment by Wire Transfer in respect to future settlements for the above
        |account, please complete your bank details below and return to us within 14 days of receipt.
        |
        |\centered Please Remit Proceeds in Dollars To:
        |
        |Payee/Account Name: [[Payee Account Name]]
        |Bank: [[Bank Name]]
        |Address: [[Bank Address: Address]]
        |Account No.: [[Bank Account Number]]
        |Sort or Swift Code or ABA No.: [[Sort or Swift Code or ABA Number]]
        |IBAN # (if applicable): [[IBAN Number]]
        |Intermediary Bank Name: [[Intermediary Bank Name]]
        |Intermediary Bank Address: [[Intermediary Bank Address: Address]]
        |
        |
        |\centered There is a fixed charge of $20.00 for payments made by wire transfer.
        |If we do not receive your bank details, payments will be remitted by check.
        |
        |Client Signature: [[Consignor Signatory Email: Identity | Signature]]
        |Dated: [[Effective Date: Date]]
        |
        |**Please note that if you complete and sign these instructions, all net proceeds for this sale location will be wired to the above referenced bank. If we do not receive these signed completed instructions, payments will be remitted by check to the above referenced address. Please note that the "Payee/Account Name" must match the consignor name.**
        |
      """.stripMargin

    compile(text)
  }

  it should "handle tables preceding other elements" in {
    val text = """this is a test

          to see if for each works

         [[texts:Collection<Text>]]

         | head1 | head2 | head3 |
         | ----- | ----- | ----- |
         {{#for each text:texts =>
         | val11 | val12 | [[text]] |
}}"""

    val template = compile(text)
    val collectionType =
      AbstractCollectionType.createParameterInstance(TextType)
    val paramValue = collectionType
      .internalFormat(
        CollectionValue(
          size = 3,
          values = Map(0 -> "text1", 1 -> "text2", 2 -> "text3"),
          collectionType
        )
      )
      .getOrThrow()

    val result = engine
      .execute(template, TemplateParameters("texts" -> paramValue))
      .getOrThrow()

    parser.forReview(result.agreements.head) shouldBe
      """<p class="no-section">this is a test<br /><br />        </p><p class="no-section">to see if for each works<br /><br />       </p><p class="no-section"><br /><br />       </p><p class="no-section"><table class="markdown-table"><tr class="markdown-table-row"><th class="markdown-table-header align-left border-show">head1</th><th class="markdown-table-header align-left border-show">head2</th><th class="markdown-table-header align-left border-show">head3</th></tr><tr class="markdown-table-row"><td class="markdown-table-data align-left border-show">val11</td><td class="markdown-table-data align-left border-show">val12</td><td class="markdown-table-data align-left border-show">text1</td></tr><tr class="markdown-table-row"><td class="markdown-table-data align-left border-show">val11</td><td class="markdown-table-data align-left border-show">val12</td><td class="markdown-table-data align-left border-show">text2</td></tr><tr class="markdown-table-row"><td class="markdown-table-data align-left border-show">val11</td><td class="markdown-table-data align-left border-show">val12</td><td class="markdown-table-data align-left border-show">text3</td></tr></table></p>""".stripMargin
  }

  it should "show an error if the constructor is of the wrong type" in {
    val text = "[[test:Text(1223)]]"

    val template = compile(text)

    engine.execute(template, TemplateParameters()) match {
      case Right(_) =>
        fail("should fail")
      case Left(ex) =>
        ex.message shouldBe "type mismatch while building the default value for type Text. the constructor result type should be OpenlawString but instead is OpenlawBigDecimal"
    }
  }

  it should "allow specifying an external call" in {

    val text =
      """[[info:OLInfo]]
        |[[param1:Text]]
        |[[param2:Text]]
        |[[externalCall:ExternalCall(
        |serviceName: "SomeIntegratedServiceName";
        |parameters:
        | param1 -> param1,
        | param2 -> param2;
        |startDate: info.now;
        |endDate: info.now + "100 days";
        |repeatEvery: '1 hour 30 minute')]]
        |[[externalCall]]
      """.stripMargin

    val Success(integratedService) =
      IntegratedServiceDefinition("""
        |[[Input:Structure(
        |param1:Text;
        |param2:Text
        |)]]
        |[[Output:Structure(
        |computationResult:Text
        |)]]
      """.stripMargin)

    val externalCallStructures =
      Map(ServiceName("SomeIntegratedServiceName") -> integratedService)
    val template = compile(text)
    engine.execute(
      template,
      TemplateParameters("param1" -> "test1", "param2" -> "test2"),
      Map.empty,
      externalCallStructures
    ) match {
      case Success(executionResult) =>
        executionResult.getExecutedVariables.map(_.name).toSet shouldBe Set(
          "info",
          "param1",
          "param2",
          "externalCall"
        )
        val Some(externalCall) = executionResult.getVariable("externalCall")
        val externalCallVarType = externalCall.varType(executionResult)
        externalCallVarType.getTypeClass shouldBe classOf[ExternalCall]
        val Some(externalCallValue) = executionResult
          .getVariableValue[ExternalCall](VariableName("externalCall"))
          .getOrThrow()
        externalCallValue
          .getServiceName(executionResult)
          .getOrThrow() shouldBe "SomeIntegratedServiceName"
        val arguments =
          externalCallValue.getParameters(executionResult).getOrThrow()
        arguments.size shouldBe 2
        arguments(VariableName("param1")).underlying shouldBe "test1"
        arguments(VariableName("param2")).underlying shouldBe "test2"
        externalCallValue
          .getStartDate(executionResult)
          .getOrThrow()
          .map(_.getEpochSecond) shouldBe Some(
          executionResult.info.now.getEpochSecond
        )

        externalCallValue
          .getEndDate(executionResult)
          .getOrThrow()
          .map(_.getEpochSecond) shouldBe Some(
          ZonedDateTime
            .ofInstant(
              executionResult.info.now,
              Clock.systemDefaultZone().getZone
            )
            .plus(100, ChronoUnit.DAYS)
            .toInstant
            .getEpochSecond
        )
        externalCallValue.getEvery(executionResult).getOrThrow() shouldBe Some(
          PeriodType.cast("1 hour 30 minute").getOrThrow()
        )
      case Left(ex) =>
        fail(ex)
    }
  }

  it should "be able to define options for Period type" in {
    val text = """[[test:Period(options: "1 day", "1 week", "2 months")]]"""

    val template = compile(text)

    val Success(result) = engine.execute(template, TemplateParameters())

    val Some(variable) = result.getVariable("test")

    variable.validate(result) shouldBe Success.unit
  }

  it should "still fail if the defined options are invalid" in {
    val text =
      """[[test:Period(options: "1 day", "1 week", "2 monthsfehwfw")]]"""

    val template = compile(text)

    val Failure(_, message) = engine.execute(template, TemplateParameters())

    message shouldBe "options element error! should be of type Period but \"2 monthsfehwfw\" is Text instead"
  }

  private def compile(text: String): CompiledTemplate =
    parser.compileTemplate(text) match {
      case Right(template) => template
      case Failure(ex, message) =>
        throw new RuntimeException(message, ex)
    }

  it should "be possible to add descriptions to properties of a Structure" in {
    val text =
      """
        |[[Person: Structure(
        |Name: Text;
        |Address: Address;
        |Member: YesNo "Is this person a member?"
        |)]]
      """.stripMargin

    val template = compile(text)

    engine.execute(template, TemplateParameters()) match {
      case Success(executionResult) =>
        val Some(structure: DefinedStructureType) =
          executionResult.findVariableType(VariableTypeDefinition("Person"))
        val field = structure.structure.typeDefinition(VariableName("Member"))
        field.description shouldBe Some("Is this person a member?")
        field.varType(executionResult) shouldBe YesNoType
      case Failure(ex, message) => fail(message, ex)
    }
  }

  it should "handle Domain correctly" in {
    val text =
      """
        [[Amount:DomainType(
         |variableType: Number;
				 |condition: this > 5;
				 |errorMessage:"amount needs to be higher than 5"
				 |)]]
         <%
         [[amount:Amount]]
         %>
      """.stripMargin

    val template = compile(text)

    engine.execute(template, TemplateParameters()) match {
      case Success(executionResult) =>
        val Some(domain: DefinedDomainType) =
          executionResult.findVariableType(VariableTypeDefinition("Amount"))
        domain.domain.typeDefinition shouldBe NumberType
      case Failure(ex, message) => fail(message, ex)
    }

    val textWithVar =
      """
        [[Amount:DomainType(
         |variableType: Number;
				 |condition: this > 5;
				 |errorMessage: "amount should be higher than 5")]]
         |
				 |[[amount:Amount]]
        |My amount is: [[amount]]
      """.stripMargin

    val template2 = compile(textWithVar)

    val Success(result) = engine.execute(template2)
    val Some(domainType: DefinedDomainType) =
      result.findVariableType(VariableTypeDefinition("Amount"))
    val Right(internalFormat) =
      domainType.internalFormat(OpenlawBigDecimal(BigDecimal("5")))
    engine.execute(template2, TemplateParameters("amount" -> internalFormat)) match {
      case Success(newResult) =>
        val text = parser.forReview(newResult.agreements.head)
        text shouldBe "<p class=\"no-section\"><br />        </p><p class=\"no-section\">5<br />My amount is: 5<br />      </p>"
      case Failure(ex, message) =>
        ex.printStackTrace()
        fail(message, ex)
    }

  }

  it should "print table properly even in a conditional" in {
    val template = compile(
      """
        |{{conditional =>
        || Optionee | Title | Address | Ethereum Address | Option Type | Number of Shares | Exercise Price | Vesting Commencement Date | Vesting Method | State of Residence |
        || --------- | --------- | --------- | --------- | --------- | --------- | --------- | --------- | ----- | --- |
        || [[Optionee 1 Name]] | [[Optionee 1 Title]] | [[Optionee 1 Address: Address]] | [[Optionee 1 Ethereum Address]] | [[Optionee 1 Option Type]] | [[Optionee 1 Shares: Number]] | [[Option Exercise Price: Number]] | [[Optionee 1 Vesting Commencement Date]] | [[Optionee 1 Vesting Method]] | [[Optionee 1 State]] |}}
      """.stripMargin
    )

    val Success(result) =
      engine.execute(template, TemplateParameters("conditional" -> "true"))
    parser.forReview(result.agreements.head) shouldBe """<p class="no-section"><br /><table class="markdown-table"><tr class="markdown-table-row"><th class="markdown-table-header align-left border-show">Optionee</th><th class="markdown-table-header align-left border-show">Title</th><th class="markdown-table-header align-left border-show">Address</th><th class="markdown-table-header align-left border-show">Ethereum Address</th><th class="markdown-table-header align-left border-show">Option Type</th><th class="markdown-table-header align-left border-show">Number of Shares</th><th class="markdown-table-header align-left border-show">Exercise Price</th><th class="markdown-table-header align-left border-show">Vesting Commencement Date</th><th class="markdown-table-header align-left border-show">Vesting Method</th><th class="markdown-table-header align-left border-show">State of Residence</th></tr><tr class="markdown-table-row"><td class="markdown-table-data align-left border-show">[[Optionee 1 Name]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 Title]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 Address]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 Ethereum Address]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 Option Type]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 Shares]]</td><td class="markdown-table-data align-left border-show">[[Option Exercise Price]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 Vesting Commencement Date]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 Vesting Method]]</td><td class="markdown-table-data align-left border-show">[[Optionee 1 State]]</td></tr></table><br />      </p>"""
  }

  it should "see when a value has been defined for a collection while validating" in {
    val template = compile(
      """\centered **Test Agreement - collection**
        |
        |[[Collection Var: Collection<Text>]] {{#for each Item: Collection Var => [[Item]]}}
        |
        |[[Email: Identity]]
      """.stripMargin
    )

    engine.execute(
      template,
      TemplateParameters(
        "Collection Var" -> "{\"values\":{\"0\":\"hello\",\"1\":\"worldooij\",\"2\":\"myoijioj\",\"3\":\"friend\"},\"size\":4}"
      )
    ) match {
      case Success(result) =>
        val Right(missingInputs) = result.allMissingInput

        missingInputs shouldBe Seq(VariableName("Email"))
      case Failure(ex, message) => fail(message, ex)
    }
  }

  it should "format small numbers properly" in {
    val text =
      """
        [[my number:Number]]
      """.stripMargin

    val template = compile(text)

    engine.execute(template, TemplateParameters("my number" -> "0.000000042")) match {
      case Success(executionResult) =>
        parser.forReview(executionResult.agreements.head) shouldBe "<p class=\"no-section\"><br />        0.000000042<br />      </p>"
      case Failure(ex, message) => fail(message, ex)
    }

    engine.execute(template, TemplateParameters("my number" -> "4200000000")) match {
      case Success(executionResult) =>
        parser.forReview(executionResult.agreements.head) shouldBe "<p class=\"no-section\"><br />        4,200,000,000<br />      </p>"
      case Failure(ex, message) => fail(message, ex)
    }
  }

  it should "not try to check for variables in other execution results if the variable is anonymous" in {

    val template =
      compile("""
        |[[my test:Collection<Text>]]
        |
        |{{#for each test: my test =>
        |^ [[test]]
        |}}
      """.stripMargin)

    val collectionType =
      AbstractCollectionType.createParameterInstance(TextType)

    engine.execute(
      template,
      TemplateParameters(
        "my test" -> collectionType
          .internalFormat(
            CollectionValue(
              size = 4,
              values = Map(
                0 -> "param 0",
                1 -> "param 1",
                2 -> "param 2",
                3 -> "param 3"
              ),
              collectionType = collectionType
            )
          )
          .getOrThrow()
      )
    ) match {
      case Success(result) =>
        parser.forReview(result.agreements.head) shouldBe "<p class=\"no-section\"><br /></p><ul class=\"list-lvl-1\"><li><p>1.  param 0<br /></p></li><li><p>2.  param 1<br /></p></li><li><p>3.  param 2<br /></p></li><li><p>4.  param 3<br /><br />      </p></li></ul>"
      case Failure(ex, message) => fail(message, ex)
    }
  }

  it should "be able to create a period from 2 dates" in {
    val text =
      """
        |[[from:DateTime]]
        |[[to:DateTime]]
        |
        |[[@period = to - from]]
        |
        |[[period]]
      """.stripMargin

    val template = compile(text)

    val fromDate = Instant.now()
    val toDate = Instant
      .now()
      .plus(2, ChronoUnit.HOURS)
      .plus(45, ChronoUnit.MINUTES)

    val Success(result) = engine.execute(
      template,
      TemplateParameters(
        "from" -> DateTimeType
          .internalFormat(OpenlawInstant(fromDate))
          .getOrThrow(),
        "to" -> DateTimeType
          .internalFormat(OpenlawInstant(toDate))
          .getOrThrow()
      )
    )

    val period = VariableName("period").evaluateT[Period](result).getOrThrow()
    period shouldBe Some(
      variableTypes.Period(hours = 2, minutes = 45)
    )

    // flip date order - should not matter
    val Right(result2) = engine.execute(
      template,
      TemplateParameters(
        "from" -> DateTimeType
          .internalFormat(OpenlawInstant(toDate))
          .getOrThrow(),
        "to" -> DateTimeType
          .internalFormat(OpenlawInstant(fromDate))
          .getOrThrow()
      )
    )

    VariableName("period")
      .evaluateT[Period](result2)
      .getOrThrow() shouldBe period
  }

  it should "not write errors if you try to print structure" in {
    val template = compile("""
        <%
        |[[Contestant Emergency Contact: Structure(
        |  Emergency Contact Name: Text;
        |  Emergency Contact Age: Number;
        |  Emergency Contact DOB: Date;
        |  Emergency Contact Address: Address
        |)]]
        |
        |# Structure type var
        |[[Medical Contact: Contestant Emergency Contact]]
        |%>
        |Name: [[Medical Contact.Emergency Contact Name]]
        |Age: [[Medical Contact.Emergency Contact Age]]
        |DOB: [[Medical Contact.Emergency Contact DOB]]
      """.stripMargin)

    val Success(result) = engine.execute(template)
    val Some(varType: DefinedStructureType) = result.findVariableType(
      VariableTypeDefinition("Contestant Emergency Contact")
    )

    val Right(internalFormat) = varType.internalFormat(
      Map[VariableName, OpenlawValue](
        VariableName("Emergency Contact Name") -> OpenlawString("David Roon"),
        VariableName("Emergency Contact Age") -> OpenlawBigDecimal(
          BigDecimal("23")
        )
      )
    )
    engine.execute(
      template,
      TemplateParameters("Medical Contact" -> internalFormat)
    ) match {
      case Success(newResult) =>
        parser.forReview(newResult.agreements.head) shouldBe "<p class=\"no-section\"><br />        <br />Name: David Roon<br />Age: 23<br />DOB: [[Medical Contact]]<br />      </p>"
      case Failure(ex, message) =>
        ex.printStackTrace()
        fail(message, ex)
    }

  }

  it should "be able to divide a period by a number" in {
    val text =
      """
        |[[period:Period]]
        |[[divisor:Number]]
        |
        |[[@newPeriod = period / divisor]]
        |
        |[[newPeriod]]
      """.stripMargin

    val template = compile(text)

    val Success(result) = engine.execute(
      template,
      TemplateParameters(
        "period" -> PeriodType
          .internalFormat(
            variableTypes.Period(
              years = 1,
              weeks = 2,
              days = 3,
              hours = 4,
              minutes = 5,
              seconds = 6
            )
          )
          .getOrThrow(),
        "divisor" -> "2"
      )
    )

    VariableName("newPeriod")
      .evaluateT[Period](result)
      .getOrThrow() shouldBe Some(
      variableTypes
        .Period(weeks = 27, days = 2, hours = 2, minutes = 2, seconds = 33)
    )

    engine.execute(
      template,
      TemplateParameters(
        "period" -> PeriodType
          .internalFormat(variableTypes.Period(years = 1))
          .getOrThrow(),
        "divisor" -> "0"
      )
    ) match {
      case Success(_) =>
        fail("should fail when dividing by zero")
      case Left(ex) =>
        ex.message shouldBe "error while evaluating the expression 'period / divisor': division by zero!"
    }

    engine.execute(
      template,
      TemplateParameters(
        "period" -> PeriodType
          .internalFormat(
            variableTypes.Period(years = 1, months = 1, weeks = 1)
          )
          .getOrThrow(),
        "divisor" -> "2"
      )
    ) match {
      case Right(_) =>
        fail("should fail when dividing a period containing a month")
      case Left(ex) =>
        ex.message shouldBe "error while evaluating the expression 'period / divisor': cannot divide months"
    }
  }

  it should "handle missing values in a structure" in {
    val template =
      compile("""\centered **Test Agreement - structure**
 |
 |# Structure definition
 |[[Contestant Emergency Contact: Structure(
 |  Emergency Contact Name: Text;
 |  Emergency Contact Age: Number;
 |  Emergency Contact DOB: Date;
 |  Emergency Contact Address: Address
 |)]]
 |
 |# Structure type var
 |[[#Medical Contact: Contestant Emergency Contact]]
 |
 |**Emergency Contact**
 |Name: [[Medical Contact.Emergency Contact Name]]
 |Age: [[Medical Contact.Emergency Contact Age]]
 |DOB: [[Medical Contact.Emergency Contact DOB]]
 |Address: [[Medical Contact.Emergency Contact Address]]
 |
 |[[Email: Identity]]
              """.stripMargin)

    engine.execute(
      template,
      TemplateParameters(
        "Medical Contact" -> "{\"Emergency Contact Name\": \"test\"}"
      )
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val text = parser.forReview(result.agreements.head)
        text shouldBe "<p class=\"align-center no-section\"><strong>Test Agreement - structure</strong></p><p class=\"no-section\"># Structure definition<br /></p><p class=\"no-section\"># Structure type var<br /></p><p class=\"no-section\"><strong>Emergency Contact</strong><br />Name: test<br />Age: [[Medical Contact]]<br />DOB: [[Medical Contact]]<br />Address: [[Medical Contact]]</p><p class=\"no-section\"><br />              </p>"
      case Failure(_, message) =>
        fail(message)
    }
  }

  it should "be possible to set a default value for an identity" in {
    val template =
      compile("""[[my identity:Identity("some@email.com")]]""".stripMargin)

    engine.execute(template) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        result.getVariableValue[Identity](VariableName("my identity")) shouldBe Success(
          Some(Identity(Email("some@email.com").getOrThrow()))
        )
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "extract all the variables and define a Structure out of it" in {
    val template =
      compile(
        """[[text var:Text]]  [[num var:Number]]  [[@num alias = num var + 5 ]]""".stripMargin
      )

    engine.execute(
      template,
      TemplateParameters("text var" -> "hello world", "num var" -> "21213")
    ) match {
      case Success(result) =>
        result.state shouldBe ExecutionFinished
        val Success(structure) = result.buildStructureFromVariablesAndAliases
        val structureType =
          AbstractStructureType.generateType(structure)
        structure.names shouldBe List(
          VariableName("text var"),
          VariableName("num var"),
          VariableName("num alias")
        )
        val Success(values) = result.buildStructureValueFromVariablesAndAliases
        values.underlying shouldBe Map[VariableName, OpenlawValue](
          VariableName("text var") -> OpenlawString("hello world"),
          VariableName("num var") -> OpenlawBigDecimal(21213),
          VariableName("num alias") -> OpenlawBigDecimal(21218)
        )
        val Success(newDefinedResult) = OpenlawExecutionState.empty
          .withVariable(VariableName("parameters"), values, structureType)

        newDefinedResult.variables.map(_.name.name) shouldBe List("parameters")
        val Success(text) =
          newDefinedResult.evaluate[OpenlawString]("parameters.text var")
        text.underlying shouldBe "hello world"
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "be possible to define expression directly between [[ ]]" in {
    val template =
      compile(
        """<%[[first name:Text]] [[last name:Text]]%>[[first name + " and " + last name]]""".stripMargin
      )

    val Success(result) = engine.execute(
      template,
      TemplateParameters("first name" -> "David", "last name" -> "Roon")
    )
    result.state shouldBe ExecutionFinished
    val text = parser.forReview(result.agreements.head)
    text shouldBe "<p class=\"no-section\">David and Roon</p>"
  }

  it should "be able to define a function type" in {
    val template =
      compile(
        """[[function variable:Function<Text>(name => name + " and all")]][[function variable("David Roon")]]""".stripMargin
      )

    val Success(result) = engine.execute(template, TemplateParameters())
    result.state shouldBe ExecutionFinished

    val text = parser.forReview(result.agreements.head)
    text shouldBe "<p class=\"no-section\">David Roon and all</p>"
  }

  it should "be able to use 'map' for collections" in {
    val template =
      compile(
        """[[collection:Collection<Text>]]
          |
          |[[@new collection = collection.map(elem => elem + " world")]]""".stripMargin
      )

    val collectionType = CollectionType(TextType)

    val Success(collectionStr) = collectionType.internalFormat(
      CollectionValue(
        size = 3,
        values = Map(0 -> "one", 1 -> "two", 2 -> "blabla"),
        collectionType = collectionType
      )
    )
    val Success(result) = engine.execute(
      template,
      TemplateParameters("collection" -> collectionStr)
    )
    result.state shouldBe ExecutionFinished

    val Success(newCollection) =
      result.evaluate[CollectionValue]("new collection")

    newCollection.values shouldBe Map(
      0 -> "one world",
      1 -> "two world",
      2 -> "blabla world"
    )
  }

  it should "be able to use 'map' for collections with numbers" in {
    val template =
      compile(
        """[[collection:Collection<Number>]]
          |
          |[[@new collection = collection.map(elem => elem + 10)]]""".stripMargin
      )

    val collectionType = CollectionType(NumberType)

    val collectionStr = collectionType
      .internalFormat(
        CollectionValue(
          size = 3,
          values = Map(
            0 -> BigDecimal("1"),
            1 -> BigDecimal("3"),
            2 -> BigDecimal("45")
          ),
          collectionType = collectionType
        )
      )
      .getOrThrow()
    val Success(result) = engine.execute(
      template,
      TemplateParameters("collection" -> collectionStr)
    )
    result.state shouldBe ExecutionFinished

    val newCollection =
      result.evaluate[CollectionValue]("new collection").getOrThrow()
    newCollection.values shouldBe Map(
      0 -> BigDecimal("11"),
      1 -> BigDecimal("13"),
      2 -> BigDecimal("55")
    )
  }

  it should "be able to use 'map' for collections with structure where we extract a field" in {
    val template =
      compile(
        """
          |[[My Structure: Structure(
          |name:Text;
          |age:Number
          |)]]
          |[[collection:Collection<My Structure>]]
          |
          |[[@new collection = collection.map(elem => elem.age)]]""".stripMargin
      )

    val Success(resultToGetType) =
      engine.execute(template, TemplateParameters())
    val Some(structureType) =
      resultToGetType.findVariableType(VariableTypeDefinition("My Structure"))
    val collectionType = CollectionType(structureType)

    val value1: Map[VariableName, OpenlawValue] =
      Map[VariableName, OpenlawValue](
        VariableName("name") -> OpenlawString("David Roon"),
        VariableName("age") -> OpenlawBigDecimal(BigDecimal(37))
      )
    val value2: Map[VariableName, OpenlawValue] =
      Map[VariableName, OpenlawValue](
        VariableName("name") -> OpenlawString("David UIhiuh"),
        VariableName("age") -> OpenlawBigDecimal(BigDecimal(47))
      )

    val collectionStr = collectionType
      .internalFormat(
        CollectionValue(
          size = 2,
          values = Map(0 -> value1, 1 -> value2),
          collectionType = collectionType
        )
      )
      .getOrThrow()
    val Success(result) = engine.execute(
      template,
      TemplateParameters("collection" -> collectionStr)
    )
    result.state shouldBe ExecutionFinished

    val newCollection =
      result.evaluate[CollectionValue]("new collection").getOrThrow()
    newCollection.values shouldBe Map(
      0 -> BigDecimal("37"),
      1 -> BigDecimal("47")
    )
    val Success(expr) = result.parseExpression("new collection")
    expr.expressionType(result).getOrThrow()
    val Success(exprType: CollectionType) = expr.expressionType(result)
    exprType.typeParameter shouldBe NumberType
  }

  it should "be able to use regex to match patterns" in {
    val template =
      compile(
        """[[regex:Regex("openlaw")]] [[some text:Text]]
          |{{regex.match(some text) => regex match with [[some text]]}}""".stripMargin
      )

    val Success(result) =
      engine.execute(template, TemplateParameters("some text" -> "openlaw"))
    result.state shouldBe ExecutionFinished

    val text = parser.forReview(result.agreements.head)
    text shouldBe """<p class="no-section"> openlaw<br />regex match with openlaw</p>"""

    val Success(result2) = engine.execute(
      template,
      TemplateParameters("some text" -> "something else")
    )
    result2.state shouldBe ExecutionFinished

    val text2 = parser.forReview(result2.agreements.head)
    text2 shouldBe """<p class="no-section"> something else<br /></p>"""
  }

  it should "be possible to append a template to an existing execution result" in {
    val template1 = compile(
      """<%[[first name:Text]] [[last name:Text]] [[age:Number]]%>""".stripMargin
    )

    val template2 =
      compile("""this is my [[first name]] [[last name]] [[age]]""".stripMargin)

    val Success(result) = engine.execute(
      template1,
      TemplateParameters(
        "first name" -> "David",
        "last name" -> "Roon",
        "age" -> "37"
      )
    )

    val Success(subResult) =
      result.withVariable(VariableName("dummy name"), None, NumberType)

    val Success(newResult) =
      engine.appendTemplateToExecutionResult(
        subResult,
        template2,
        (_, _) => None
      )
    newResult.agreements.size shouldBe 1
    newResult.state shouldBe ExecutionFinished

    val text = parser.forReview(newResult.agreements.head)
    text shouldBe "<p class=\"no-section\">this is my David Roon 37</p>"

    val text2 = parser.forReview(result.agreements.head)
    text2 shouldBe ""
  }

  it should "work with special type in clause" in {
    val mainTemplate = compile(
      """
        |[[Doc: Choice("NDA", "MOU")]]
        |[[I want to sign: Doc]]
        |
        |{{
        |    {{I want to sign = "NDA" => [[NDA: Clause("Mutual Confidentiality Agreement")]]}}
        |    {{I want to sign = "MOU" => [[MOU: Clause("Simple MOU")]]}}
        |}}
        |""".stripMargin
    )

    val clause1 = compile(
      """<%
                            |[[State: Choice("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Northern Mariana Islands","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virgin Island","Virginia","Washington","West Virginia","Wisconsin","Wyoming")]]
                            |%>
                            |<%
                            |[[Entity: Choice("corporation", "limited liability company", "limited partnership", "public benefit corporation")]]
                            |%>
                            |<%
                            |# Variables arranged by headers:
                            |==Describe Party A to NDA:==
                            |[[PartyA Name "Name?"]]
                            |[[PartyA Corp "Legal entity?"]]
                            |[[PartyA Jurisdiction: State("Delaware") "Where was legal entity formed?"]]
                            |[[PartyA Corporate Form: Entity("corporation") "What type of legal entity?"]]
                            |[[PartyA Signatory Email "Email for signature?"]]
                            |
                            |==Describe Party B to NDA:==
                            |[[PartyB Name "Name?"]]
                            |[[PartyB Corp "Legal entity?"]]
                            |[[PartyB Jurisdiction: State("Delaware") "Where was legal entity formed?"]]
                            |[[PartyB Corporate Form: Entity("corporation") "What type of legal entity?"]]
                            |[[PartyB Signatory Email "Email for signature?"]]
                            |
                            |==Other:==
                            |[[Jurisdiction of Choice: State("Delaware") "What state law should govern this NDA?"]]
                            |%>
                            |
                            |\centered **Mutual Confidentiality Agreement**
                            |
                            |This Mutual Confidentiality Agreement (this "**Agreement**"), effective as of the later of the two dates appearing on the signature page below (the "**Effective Date**"), is between [[PartyA Name]]{{PartyA Corp =>, a [[PartyA Jurisdiction]] [[PartyA Corporate Form]]}} ("**Party A**"), and [[PartyB Name]]{{PartyB Corp =>, a [[PartyB Jurisdiction]] [[PartyB Corporate Form]]}} ("**Party B**"), regarding a potential strategic relationship (the "**Relationship**"). This Agreement refers to Party A and Party B collectively as the "**Parties**" and each, individually, a "**Party**."
                            |
                            |WHEREAS, the Parties wish to discuss the possibility of a mutually beneficial business arrangement (the "**Purpose**").
                            |
                            |WHEREAS, the Parties must share certain information that is non-public proprietary or confidential to carry out the Purpose.
                            |
                            |WHEREAS, the Parties acknowledge that the benefit of receiving such information shall be sufficient basis for entering into this Agreement.
                            |
                            |NOW, THEREFORE, in consideration of the mutual covenants, terms and conditions set forth herein, and for other good and valuable consideration, the receipt and sufficiency of which are hereby acknowledged, the Parties agree as follows:
                            |
                            |^(General)In connection with the Purpose, either party ("**Disclosing Party**") may disclose Confidential Information (as defined below) to the other party ("**Recipient**"). Recipient shall use the Confidential Information solely for the Purpose and, subject to Section [[Obligatory Disclosure]] below, shall not disclose such Confidential Information other than to its employees, contractors, officers, directors, attorneys, accountants, and financial advisors (collectively, "**Representatives**") who: (a) need access to such Confidential Information for the Purpose; (b) are informed of its confidential nature; and (c) are bound by confidentiality obligations no less protective of the Confidential Information than the terms contained herein. Recipient shall safeguard the Confidential Information from unauthorized use, access, or disclosure using no less than a commercially reasonable degree of care. Recipient will be responsible for any breach of this Agreement caused by its Representatives. Recipient agrees to notify Disclosing Party in writing of any misuse or misappropriation of the Confidential Information of Disclosing Party that may come to Recipient’s attention.
                            |
                            |^(Confidential Information)"**Confidential Information**" means all non-public proprietary or confidential information of Disclosing Party, in oral, visual, written, electronic, or other tangible or intangible form, whether or not marked or designated as "confidential," that contain any of the following:
                            |
                            |^^all information concerning the past, present and future business affairs of the Disclosing Party and its affiliates and of their customers, suppliers and other third parties, including, without limitation, finances, customer information, supplier information, products, services, organizational structure and internal practices, forecasts, sales and other financial results, records and budgets, and business, marketing, development, sales and other commercial strategies;
                            |
                            |^^the Disclosing Party’s unpatented inventions, ideas, methods and discoveries, trade secrets, know-how, unpublished patent applications and other confidential intellectual property;
                            |
                            |^^all designs, specifications, documentation, components, source code, object code, images, icons, audiovisual components and objects, schematics, drawings, protocols, processes, and other visual depictions, in whole or in part, of any of the foregoing;
                            |
                            |^^any third-party confidential information included with, or incorporated in, any information provided by the Disclosing Party to the Recipient or its Representatives; and
                            |
                            |^^all notes, analyses, compilations, reports, forecasts, studies, samples, data, statistics, summaries, interpretations and other materials (collectively, the "**Notes**") prepared by or for the Recipient or its Representatives that contain, are based on, or otherwise reflect or are derived from, in whole or in part, any of the foregoing.
                            |
                            |^(Exceptions) Except as required by applicable federal, state or local law or regulation, Confidential Information shall not include information that:
                            |
                            |^^is or becomes generally available to the public other than as a result of Recipient’s or its Representatives’ breach of this Agreement;
                            |
                            |^^is obtained by Recipient or its Representatives on a non-confidential basis from a third-party that was not legally or contractually restricted from disclosing such information;
                            |
                            |^^was in Recipient’s or its Representatives’ possession prior to Disclosing Party’s disclosure hereunder; or
                            |
                            |^^was or is independently developed by Recipient or its Representatives without using any Confidential Information.
                            |
                            |^(Obligatory Disclosure)If Recipient or any of its Representatives is required by applicable law or a valid legal order to disclose any Confidential Information, Recipient shall notify Disclosing Party of such requirements so that Disclosing Party may seek, at Disclosing Party’s expense, a protective order or other remedy, and Recipient shall reasonably assist Disclosing Party therewith. If Recipient remains legally compelled to make such disclosure, it shall: (a) only disclose that portion of the Confidential Information that it is required to disclose; and (b) use reasonable efforts to ensure that such Confidential Information is afforded confidential treatment.
                            |
                            |^(Return)On Disclosing Party’s request, Recipient shall, at Disclosing Party’s discretion, promptly return to Disclosing Party or destroy all Confidential Information in its and its Representatives’ possession other than Notes, and destroy all Notes; provided, however, that Recipient may retain copies of Confidential Information that are stored on Recipient’s IT backup and disaster recovery systems until the ordinary course of deletion thereof. Recipient shall continue to be bound by the terms and conditions of this Agreement with respect to such retained Confidential Information.
                            |
                            |^(Representations)This Agreement imposes no obligation on either party to disclose any Confidential Information or to negotiate for, enter into, or otherwise pursue the Purpose. Disclosing Party makes no representation or warranty, expressed or implied, as to the accuracy or completeness of the Confidential Information, and will have no liability to Recipient or any other person relating to Recipient’s use of any of the Confidential Information or any errors therein or omissions therefrom.
                            |
                            |^(Title)Disclosing Party retains its entire right, title, and interest in and to all Confidential Information, and no disclosure of Confidential Information hereunder will be construed as a license, assignment, or other transfer of any such right, title, and interest to Recipient or any other person.
                            |
                            |^(Term)The rights and obligations of the parties under this Agreement expire three years after the Effective Date; provided that with respect to Confidential Information that constitutes a trade secret under applicable law, such rights and obligations will survive such expiration until, if ever, such Confidential Information loses its trade secret protection other than due to an act or omission of Recipient or its Representatives.
                            |
                            |^(Breach)Recipient acknowledges and agrees that any breach of this Agreement will cause irreparable harm and injury to Disclosing Party for which money damages would be an inadequate remedy and that, in addition to remedies at law, Disclosing Party is entitled to equitable relief as a remedy for any such breach. Recipient waives any claim or defense that Disclosing Party has an adequate remedy at law in any such proceeding. Nothing herein shall limit the equitable or available remedies at law for Disclosing Party.
                            |
                            |^(Jurisdiction)The laws of [[Jurisdiction of Choice]] shall govern this Agreement and all matters relating to it, including its construction, without regard to any conflict of laws provisions. A party wishing to institute any legal suit, action, or proceeding relating to this Agreement must do so in a(n) [[Jurisdiction of Choice]] court. Each Party irrevocably submits to the exclusive jurisdiction of such courts in any such suit, action, or proceeding.
                            |
                            |^(Notice)All notices must be in writing and may either be sent prepaid by nationally recognized courier, or via certified or registered mail, return receipt requested, to the relevant party at the address set forth in the preamble, or to the email address appearing with each Party’s authorized signature, below. All notices are effective upon actual receipt.
                            |
                            |^(Complete)This Agreement constitutes the entire agreement of the parties with respect to its subject matter, and supersedes all prior and contemporaneous understandings, agreements, representations, and warranties, whether written or oral, with respect to such subject matter. This Agreement may only be amended, modified, waived, or supplemented by an agreement in writing signed by both parties.
                            |
                            |IN WITNESS WHEREOF, the Parties hereto have executed this Agreement as of the Effective Date hereof.
                            |
                            |**[[PartyA Name | Uppercase]]**
                            |
                            |[[PartyA Signatory Email: Identity | Signature]]
                            |______________________________
                            |{{PartyA Corp => Title: Authorized Signatory}}
                            |
                            |**[[PartyB Name | Uppercase]]**
                            |
                            |[[PartyB Signatory Email: Identity | Signature]]
                            |______________________________
                            |{{PartyB Corp => Title: Authorized Signatory}}""".stripMargin
    )

    val Success(_) = engine.execute(
      mainTemplate,
      TemplateParameters("I want to sign" -> "NDA"),
      Map(
        TemplateSourceIdentifier(
          TemplateTitle("Mutual Confidentiality Agreement")
        ) -> clause1
      )
    )
  }

  it should "let us append date type to string" in {
    val template =
      compile("""<%
        |[[text:Text]]
        |[[number:Number]]
        |[[address:EthAddress]]
        |[[date:Date]]
        |%>
        |[[text + number]] [[text  + address]] [[text + date]]
        |[[number + text]] [[address  + text]] [[date + text]]""".stripMargin)

    val now = Instant.now()

    val result = engine
      .execute(
        template,
        TemplateParameters(
          "text" -> "hello world",
          "number" -> "1234",
          "date" -> DateType
            .internalFormat(now)
            .getOrThrow(),
          "address" -> EthAddressType
            .internalFormat(
              EthereumAddress("0xcFc2206eAbFDc5f3d9e7fA54f855A8C15D196c05")
                .getOrThrow()
            )
            .getOrThrow()
        )
      )
      .getOrThrow()

    val text = parser.forReview(result.agreements.head)

    val nowFormatted: String = SimpleDateFormatter
      .stringFormat(
        StringConstant(""),
        OpenlawInstant(now),
        result
      )
      .getOrThrow()

    text shouldBe s"""<p class="no-section"><br />hello world1,234 hello worldcfc2206eabfdc5f3d9e7fa54f855a8c15d196c05 hello world${nowFormatted}<br />1234hello world 0xcfc2206eabfdc5f3d9e7fa54f855a8c15d196c05hello world ${nowFormatted}</p>"""
  }

  it should "be possible to define a structure within a structure" in {
    val template =
      compile("""
        |[[struct a: Structure(
        |a: Text;
        |b: Number
        |)]]
        |
        |[[struct b:Structure(
        |a: struct a;
        |b: struct a;
        |c: Text)]]
        |
        |[[some value:struct b]]
        |""".stripMargin)

    val result = engine.execute(template).getOrThrow()

    result.buildStructureValueFromVariablesAndAliases.getOrThrow()
  }

  it should "check slow execution" in {
    val startTime = System.currentTimeMillis()
    val template = compile(
      """
        |<%
        |[[Effective Date: Date]]
        |[[@Maturity Date = Effective Date + "2 years"]]
        |[[@Closing Date = Effective Date + "6 months"]]
        |[[Lender Info: Structure(
        |  First Name: Text;
        |  Last Name: Text;
        |  Investment Amount: Number;
        |  Lender Address: Address)]]
        |[[Lenders: Collection<Lender Info>]]
        |
        |
        |==Company Information==
        |[[Company Name]]
        |[[Company Address: Address]]
        |[[State of Incorporation]]
        |[[CEO First Name]]
        |[[CEO Last Name]]
        |[[Number of Issued Shares: Number]]
        |
        |==Terms==
        |[[Effective Date: Date]]
        |[[Valuation Cap:Number]]
        |[[Maturity Conversion Cap:Number]]
        |[[Amount of Raise:Number]]
        |
        |==Lender Info==
        |[[Lenders]]
        |
        |==Other==
        |[[State of Governing Law]]
        |
        |%>
        |
        |\centered **NOTE PURCHASE AGREEMENT**
        |
        |[[Effective Date]] [[Company Name]] [[State of Incorporation]]
        |
        |^(Definition (style:decimal)) **__Definitions__**.
        |
        |^^(_(symbol: 'Decimal')) 
        |
        |^^ "__Conversion Shares__" 
        |
        |^^ "__Corporate Transaction__"
        |
        |^^ "__Equity Securities__"
        |
        |^^ "__Initial Public Offering__" 
        |
        |^^ "__Maturity Date__" [[Maturity Date]]. 
        |
        |^^ "__Next Equity Financing__" 
        |
        |^^ "__Notes__" 
        |
        |^^ "__Requisite Noteholders__" 
        |
        |^^ "__Securities Act__"
        |
        |^ **__Terms of the Notes__**.
        |
        |^^ **__Issuance of Notes__**.  [[Right to Convert]]
        |
        |^^(Right to Convert) **__Right to Convert Notes__**.
        |
        |^^^(_(symbol: 'LowerLetter')) 
        |
        |^^^ [[Number of Issued Shares]] $[[Maturity Conversion Cap]].
        |
        |^^^ [[Number of Issued Shares]] $[[Maturity Conversion Cap]].
        |
        |^^^ __No Fractional Shares__. 
        |
        |^^^ __Mechanics of Conversion__
        |
        |^ **__Closing Mechanics__**.
        |
        |^^ **__Closing__**. 
        |
        |^^ **__Subsequent Closings__**. [[Closing Date]] [[Amount of Raise]] [[Entire Agreement]] 
        |
        |^ **__Representations and Warranties of the Company__**.
        |
        |^^ **__Organization, Good Standing and Qualification__**.
        |
        |^^ **__Authorization__**. 
        |
        |^^ **__Compliance with Other Instruments__**. 
        |
        |^^ **__Valid Issuance of Stock__**. 
        |
        |^ **__Representations, Warranties and Additional Agreements of the Lenders__**.
        |
        |^^ **__Representations and Warranties of the Lenders__**
        |
        |^^^ __Authorization__. 
        |
        |^^^ __Purchase Entirely for Own Account__. 
        |
        |^^^ __Disclosure of Information__. 
        |
        |^^^ __Investment Experience__. 
        |
        |^^^ __Accredited Investor__. 
        |
        |^^^ __Restricted Securities__. 
        |
        |^^(Further Limitations and Disposition) **__Further Limitations on Disposition__**. [[Further Limitations and Disposition]], [[Market Standoff Agreement]]
        |
        |^^^ 
        |
        |^^^ 
        |
        |^^ **__Legends__**. 
        |
        |^^ **__Bad Actor Representations and Covenants__**. 
        |
        |^^ **__Exculpation Among Lenders__**. 
        |
        |^ **__Defaults and Remedies__**.
        |
        |^^ **__Events of Default__**. 
        |
        |^^^ 
        |
        |^^^ 
        |
        |^^^ 
        |
        |^^^ 
        |
        |^^ **__Remedies__**. 
        |
        |^ **__Miscellaneous__**.
        |
        |^^ **__Successors and Assigns__**. 
        |
        |^^ **__Governing Law__**. [[State of Governing Law]] [[State of Governing Law]].
        |
        |^^ **__Counterparts; Delivery__**. 
        |
        |^^ **__Titles and Subtitles__**. 
        |
        |^^(Notices) **__Notices__**. [[Notices]]
        |
        |If to the Company:
        |
        |\indent [[Company Name]]
        |\indent [[Company Address]]
        |\indent Attention: Chief Executive Officer
        |
        |
        |^^ **__Finder’s Fee__**. 
        |
        |^^ **__Expenses__**. 
        |
        |^^(Entire Agreement) **__Entire Agreement; Amendments and Waivers__**. 
        |
        |^^ **__Effect of Amendment or Waiver__**. 
        |
        |^^ **__Severability__**. 
        |
        |^^(Market Standoff Agreement) **__Market Stand-Off Agreement__**. 
        |
        |[[Market Standoff Agreement]]):
        |
        |^^ **__Stock Purchase Agreement__**. 
        |
        |^^ **__Exculpation Among Lenders__**. 
        |
        |^^ **__Further Assurance__**. 
        |
        |^^ **__Waiver of Jury Trial__**. 
        |
        |^^ **__Survival__**. 
        |
        |^^ **__Waiver of Conflicts__**. 
        |
        |\centered [*signature pages follow*]
        |
        |\sectionbreak
        |
        |\right-three-quarters [[Company Name | Uppercase]]
        |
        |\right-three-quarters By: ______________________
        |\right-three-quarters Name: [[CEO First Name]] [[CEO Last Name]]
        |\right-three-quarters Title: Chief Executive Officer
        |
        |\pagebreak
        |
        |{{#for each Lender: Lenders =>
        |\indentIN WITNESS WHEREOF, the parties have executed this Agreement as of the date first above written.
        |
        |Dated as of __[[Effective Date]]__
        |
        |\right-three-quarters **LENDER**
        |
        |\right-three-quarters _________________________________________
        |\right-three-quarters [[Lender.First Name]] [[Lender.Last Name]]
        |\right-three-quarters [[Lender.Lender Address]]
        |
        |\pagebreak
        |
        |}}
        |
        |\centered **__SCHEDULE OF LENDERS__**
        |
        || Lender | Closing / Subsequent Closing Date | Principal Balance of Promissory Note |
        || --------- | --------- | --------- |
        |{{#for each Lender: Lenders =>
        || [[Lender.First Name]] [[Lender.Last Name]] | [[Effective Date]] / [[Closing Date]] | $[[Lender.Investment Amount]] |
        |}}
        |
        |""".stripMargin
    )

    val compileTime = System.currentTimeMillis()

    val executionResult = engine.execute(template).getOrThrow()
    val executionTime = System.currentTimeMillis()
    parser.forReview(executionResult.agreements.head)

    val renderTime = System.currentTimeMillis()

    val json = executionResult.toSerializable.asJson.noSpaces

    decode[SerializableTemplateExecutionResult](json) match {
      case Right(ser) =>
      case Left(err) =>
        err.printStackTrace()
        fail(err.getMessage)
    }

    println(s"compilation time: ${compileTime - startTime}ms")
    println(s"execution time: ${executionTime - compileTime}ms")
    println(s"render time: ${renderTime - executionTime}ms")
  }

  it should "work fine with event filters" in {
    val template = compile(
      """<%
                             |[[info: OLInfo]]
                             |%>
                             |__*[[Signer Email: Identity | Signature]]*__
                             |0x[[Signer EthAddress: EthAddress]]
                             |\right *[[info.id]]*
                             |[[Post:EthereumCall(
                             |contract:"0xE735B7Ca5F4865A688Df3d3a49A462066f24ff2A";
                             |interface:[{"anonymous":false,"inputs":[{"indexed":true,"internalType":"string","name":"ID","type":"string"}],"name":"Post","type":"event"},{"constant":true,"inputs":[],"name":"ID","outputs":[{"internalType":"string","name":"","type":"string"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"internalType":"string","name":"_ID","type":"string"}],"name":"post","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"}];
                             |network:"Rinkeby";
                             |from:Signer EthAddress;
                             |function:"post";
                             |arguments:info.id)]]
                             |[[Post Data]]
                             |[[Post Event: EthereumEventFilter(
                             |  contract address:"0xE735B7Ca5F4865A688Df3d3a49A462066f24ff2A";
                             |  interface:[{"anonymous":false,"inputs":[{"indexed":true,"internalType":"string","name":"ID","type":"string"}],"name":"Post","type":"event"}];
                             |  event type name: "Post";
                             |  conditional filter: this.ID = Post Data)]]
                             |This event value is: [[Post Event.event.ID]]""".stripMargin
    )
    val Success(result) = engine.execute(template)
    val Success(validationResult) = result.validateExecution
    validationResult.validationExpressionErrors shouldBe Nil
  }

  it should "be possible to re-define a formatter" in {
    val template =
      compile("""[[some value:Identity | signature]]""".stripMargin)

    val someNewFormatter: Formatter = new Formatter {
      override def format(
          expression: Expression,
          value: OpenlawValue,
          executionResult: TemplateExecutionResult
      ): Result[List[
        AgreementElement
      ]] = value match {
        case Identity(email) =>
          executionResult.signatureProofs
            .get(email)
            .map(p => Success(List(FreeText(Text(s"s/${p.fullName}")))))
            .getOrElse(
              Success(
                List(
                  FreeText(Text(s"{{signature of ${expression.toString} }}"))
                )
              )
            )
        case _ =>
          Failure("it should be an identity, nothing else!")
      }

      override def stringFormat(
          expression: Expression,
          value: OpenlawValue,
          executionResult: TemplateExecutionResult
      ): Result[
        String
      ] = Success("")

      override def missingValueFormat(
          expression: Expression
      ): List[
        AgreementElement
      ] = List(FreeText(Text(s"{{signature of $expression }}")))
    }

    val result = engine
      .execute(
        template,
        TemplateParameters(
          "some value" -> IdentityType
            .internalFormat(Identity(Email("some@email.com").getOrThrow()))
            .getOrThrow()
        ),
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        None,
        None,
        None,
        (definition, _) => {
          definition match {
            case Some(FormatterDefinition("signature", _)) =>
              Some(someNewFormatter)
            case _ => None
          }
        }
      )
      .getOrThrow()

    val initialResult = engine
      .execute(
        template,
        TemplateParameters(
          "some value" -> IdentityType
            .internalFormat(Identity(Email("some@email.com").getOrThrow()))
            .getOrThrow()
        )
      )
      .getOrThrow()

    parser.forReview(initialResult.agreements.head) shouldBe "<p class=\"no-section\">{{signature of some value}}</p>"

    parser.forReview(result.agreements.head) shouldBe "<p class=\"no-section\">{{signature of some value }}</p>"
  }
}
