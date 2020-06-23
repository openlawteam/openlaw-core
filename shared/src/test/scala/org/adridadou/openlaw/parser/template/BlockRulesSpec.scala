package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.parser.template.variableTypes.YesNoType
import org.scalatest.{EitherValues, FlatSpec, Matchers, TryValues}
import org.parboiled2.{ErrorFormatter, ParseError, ParserInput}

import scala.util.{Failure, Success, Try}

class BlockRulesSpec
    extends FlatSpec
    with Matchers
    with TryValues
    with EitherValues {

  case class TestParser(string: String) extends BlockRules {
    val input = ParserInput(string)

    def BlockRule = rule { blockRule ~ EOI }
    def TableColumnEntry = rule { tableColumnEntry ~ EOI }
    def TableRow = rule { tableRow ~ EOI }
    def TableHeaderBreakString = rule { tableHeaderBreakString ~ EOI }
    def TableHeaderBreak = rule { tableHeaderBreak ~ EOI }
    def Table = rule { tableKey ~ EOI }
  }

  "The parser" should "parse a table column entry" in {
    TestParser("").TableColumnEntry.run() shouldBe a[Failure[_]]
    TestParser(" ").TableColumnEntry.run() shouldBe a[Failure[_]]
    TestParser("|").TableColumnEntry.run() shouldBe a[Failure[_]]
    TestParser("| ").TableColumnEntry.run() shouldBe a[Failure[_]]
    TestParser("col1").TableColumnEntry.run().success.value.elems shouldBe List(
      Text("col1")
    )
    TestParser(" col1").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe List(
      Text("col1")
    )
    TestParser("col1 ").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe List(
      Text("col1")
    )
    TestParser(" col1 ").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe List(
      Text("col1")
    )
    TestParser("col1 with spaces").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe List(Text("col1 with spaces"))
  }

  it should "parse a table column entry with a variable" in {
    TestParser("[[var1]]").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe Vector(
      VariableDefinition("var1")
    )
    TestParser("[[var1]] ").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe Vector(VariableDefinition("var1"))
    TestParser(" [[var1]] ").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe Vector(VariableDefinition("var1"))
    TestParser(" [[var1]]").TableColumnEntry
      .run()
      .success
      .value
      .elems shouldBe Vector(VariableDefinition("var1"))
  }

  private def conditionalBlock(text: String): Try[ConditionalBlock] =
    TestParser("""{{conditional1 "Question" => Question text}}""").TableColumnEntry
      .run()
      .success
      .value
      .elems
      .head match {
      case c: ConditionalBlock => Success(c)
      case _                   => Failure(new RuntimeException("wrong type"))
    }

  it should "parse a table column entry with a conditional" in {
    val Success(value1) =
      conditionalBlock("""{{conditional1 "Question" => Question text}}""")
    value1.block shouldBe Block(List(TemplateText(List(Text("Question text")))))
    value1.conditionalExpression shouldBe VariableDefinition(
      VariableName("conditional1"),
      Some(VariableTypeDefinition(YesNoType.name)),
      Some("Question")
    )

    val Success(value2) =
      conditionalBlock(""" {{conditional1 "Question" => Question text}}""")
    value2.block shouldBe Block(List(TemplateText(List(Text("Question text")))))
    value2.conditionalExpression shouldBe VariableDefinition(
      VariableName("conditional1"),
      Some(VariableTypeDefinition(YesNoType.name)),
      Some("Question")
    )

    val Success(value3) =
      conditionalBlock(""" {{conditional1 "Question" => Question text}} """)
    value3.block shouldBe Block(List(TemplateText(List(Text("Question text")))))
    value3.conditionalExpression shouldBe VariableDefinition(
      VariableName("conditional1"),
      Some(VariableTypeDefinition(YesNoType.name)),
      Some("Question")
    )

    val Success(value4) =
      conditionalBlock("""{{conditional1 "Question" => Question text}} """)
    value4.block shouldBe Block(List(TemplateText(List(Text("Question text")))))
    value4.conditionalExpression shouldBe VariableDefinition(
      VariableName("conditional1"),
      Some(VariableTypeDefinition(YesNoType.name)),
      Some("Question")
    )

  }

  it should "parse a table row" in {
    TestParser("|col1|col2|col3|").TableRow
      .run()
      .success
      .value should contain allOf (TableBlock(List(Text("col1"))), TableBlock(
      List(Text("col2"))
    ), TableBlock(List(Text("col3"))))
    TestParser("| col1 | col2 | col3|").TableRow
      .run()
      .success
      .value should contain allOf (TableBlock(List(Text("col1"))), TableBlock(
      List(Text("col2"))
    ), TableBlock(List(Text("col3"))))
    TestParser("| col1 | col2 | col3 |").TableRow
      .run()
      .success
      .value should contain allOf (TableBlock(List(Text("col1"))), TableBlock(
      List(Text("col2"))
    ), TableBlock(List(Text("col3"))))
    TestParser("| col1 | col2 | col3 |").TableRow
      .run()
      .success
      .value should contain allOf (TableBlock(List(Text("col1"))), TableBlock(
      List(Text("col2"))
    ), TableBlock(List(Text("col3"))))
    TestParser("| col1 | col2 | col3|").TableRow
      .run()
      .success
      .value should contain allOf (TableBlock(List(Text("col1"))), TableBlock(
      List(Text("col2"))
    ), TableBlock(List(Text("col3"))))
  }

  it should "parse a header break string" in {
    TestParser("---").TableHeaderBreakString.run() shouldBe a[Success[_]]
    TestParser(":--").TableHeaderBreakString.run() shouldBe a[Success[_]]
    TestParser("--:").TableHeaderBreakString.run() shouldBe a[Success[_]]
    TestParser("==:").TableHeaderBreakString.run() shouldBe a[Success[_]]
    TestParser(":-:").TableHeaderBreakString.run() shouldBe a[Success[_]]
    TestParser(":=-").TableHeaderBreakString.run() shouldBe a[Failure[_]]
    TestParser(":-=").TableHeaderBreakString.run() shouldBe a[Failure[_]]
    TestParser("::-").TableHeaderBreakString.run() shouldBe a[Failure[_]]
    TestParser("::--").TableHeaderBreakString.run() shouldBe a[Failure[_]]
    TestParser("::").TableHeaderBreakString.run() shouldBe a[Failure[_]]
    TestParser("-::-").TableHeaderBreakString.run() shouldBe a[Failure[_]]
    TestParser("--::").TableHeaderBreakString.run() shouldBe a[Failure[_]]
    TestParser("a--").TableHeaderBreakString.run() shouldBe a[Failure[_]]
  }

  it should "parse a table header break" in {
    TestParser("| ---  |  ---  | ---   |").TableHeaderBreak
      .run() shouldBe a[Success[_]]
    TestParser("| --- | --- | --- |").TableHeaderBreak
      .run() shouldBe a[Success[_]]
    TestParser("|:-- | --: | :-:|").TableHeaderBreak
      .run() shouldBe a[Success[_]]
    TestParser("|::- | --- | ---|").TableHeaderBreak
      .run() shouldBe a[Failure[_]]
    TestParser("|::- | --- | ---|").TableHeaderBreak
      .run() shouldBe a[Failure[_]]
    TestParser("|:: | --- | ---|").TableHeaderBreak.run() shouldBe a[Failure[_]]
    TestParser("|: | --- | ---|").TableHeaderBreak.run() shouldBe a[Failure[_]]
  }

  it should "parse a table construct" in {
    val table = "| head1 | head2 | head3 | \n" +
      "| --- | ==: | :-: |\n" +
      "| entry11 | entry12 | entry13 |\n" +
      "| entry21 | entry22 | entry23 |\n"

    val expected = List(
      Table(
        List(
          TableBlock(List(Text("head1"))),
          TableBlock(List(Text("head2"))),
          TableBlock(List(Text("head3")))
        ),
        List(
          LeftAlignment -> ShowBorder,
          RightAlignment -> HideBorder,
          CenterAlignment -> ShowBorder
        ),
        List(
          List(
            TableBlock(List(Text("entry11"))),
            TableBlock(List(Text("entry12"))),
            TableBlock(List(Text("entry13")))
          ),
          List(
            TableBlock(List(Text("entry21"))),
            TableBlock(List(Text("entry22"))),
            TableBlock(List(Text("entry23")))
          )
        )
      )
    )
    checkResult(TestParser(table), TestParser(table).Table.run()) shouldBe expected
  }

  it should "parse a table construct with header and no rows" in {
    val table =
      "| Lender | Closing / Subsequent Closing Date | Principal Balance of Promissory Note |\n" +
        "| --------- | --------- | --------- |\n"

    checkResult(TestParser(table), TestParser(table).Table.run()) shouldBe List(
      Table(
        List(
          TableBlock(List(Text("Lender"))),
          TableBlock(List(Text("Closing / Subsequent Closing Date"))),
          TableBlock(List(Text("Principal Balance of Promissory Note")))
        ),
        List(
          LeftAlignment -> ShowBorder,
          LeftAlignment -> ShowBorder,
          LeftAlignment -> ShowBorder
        ),
        Nil
      )
    )

    checkResult(TestParser(table), TestParser(table).BlockRule.run()) shouldBe Block(
      List(
        TemplateText(
          List(
            Table(
              List(
                TableBlock(List(Text("Lender"))),
                TableBlock(List(Text("Closing / Subsequent Closing Date"))),
                TableBlock(List(Text("Principal Balance of Promissory Note")))
              ),
              List(
                LeftAlignment -> ShowBorder,
                LeftAlignment -> ShowBorder,
                LeftAlignment -> ShowBorder
              ),
              Nil
            )
          )
        )
      )
    )
  }

  it should "let you use formatting in the table content" in {
    val table =
      """|| --------- | ----------- |
        || **Customer**: | Customer Name | 
        || **Customer Contact Person**: | Customer Contact Person | 
        || **Customer Phone Number**: | Customer Phone Number |
        || **Customer Email Address**: | Customer Email Address |""".stripMargin

    val b =
      TemplateText(
        List(
          Table(
            Nil,
            List((LeftAlignment, ShowBorder), (LeftAlignment, ShowBorder)),
            List(
              List(
                TableBlock(List(Strong, Text("Customer"), Strong, Text(":"))),
                TableBlock(List(Text("Customer Name")))
              ),
              List(
                TableBlock(
                  List(
                    Strong,
                    Text("Customer Contact Person"),
                    Strong,
                    Text(":")
                  )
                ),
                TableBlock(List(Text("Customer Contact Person")))
              ),
              List(
                TableBlock(
                  List(Strong, Text("Customer Phone Number"), Strong, Text(":"))
                ),
                TableBlock(List(Text("Customer Phone Number")))
              ),
              List(
                TableBlock(
                  List(
                    Strong,
                    Text("Customer Email Address"),
                    Strong,
                    Text(":")
                  )
                ),
                TableBlock(List(Text("Customer Email Address")))
              )
            )
          )
        )
      )

    checkResult(TestParser(table), TestParser(table).BlockRule.run())
      .elems(0) shouldBe b
  }

  it should "parse a table construct with loop" in {
    val table =
      "| Lender | Closing / Subsequent Closing Date | Principal Balance of Promissory Note |\n" +
        "| --------- | --------- | --------- |\n" +
        "{{#for each Lender: Lenders =>\n" +
        "| [[Lender.First Name]] [[Lender.Last Name]] | [[Effective Date]] / [[Closing Date]] | $[[Lender.Investment Amount]] |\n" +
        "}}\n"

    val b =
      TemplateText(
        List(
          Table(
            List(
              TableBlock(List(Text("Lender"))),
              TableBlock(List(Text("Closing / Subsequent Closing Date"))),
              TableBlock(List(Text("Principal Balance of Promissory Note")))
            ),
            List(
              (LeftAlignment, ShowBorder),
              (LeftAlignment, ShowBorder),
              (LeftAlignment, ShowBorder)
            ),
            Nil
          )
        )
      )

    checkResult(TestParser(table), TestParser(table).BlockRule.run())
      .elems(0) shouldBe b
  }

  private def checkResult[T](compiler: TestParser, result: Try[T]): T =
    result match {
      case Failure(ex: ParseError) =>
        fail(compiler.formatError(ex, new ErrorFormatter(showTraces = true)))
      case Failure(ex) => fail(ex.toString)
      case Success(t)  => t
    }
}
