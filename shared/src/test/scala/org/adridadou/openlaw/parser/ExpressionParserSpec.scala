package org.adridadou.openlaw.parser

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.{BooleanExpression, ComparisonExpression, ParensExpression}
import org.adridadou.openlaw.result.{Failure, Success}
import org.scalatest._

/**
  * Created by davidroon on 05.05.17.
  */
class ExpressionParserSpec  extends FlatSpec with Matchers {

  private val service = new ExpressionParserService()

  it should "parse simple expression" in {
    val text = "variable 1 + 23"

    service.parseExpression(text) match {
       case Success(result) => result.toString shouldBe text
       case Failure(_, message) => fail(message)
    }
  }

  it should "parse simple string expression" in {
    val text = """variable 1 + "hello world""""

    service.parseExpression(text) match {
      case Success(result) => result.toString shouldBe text
      case Failure(_, message) => fail(message)
    }
  }

  it should "parse simple expression 2" in {
    val text = "23 + variable 1"

    service.parseExpression(text) match {
      case Success(result) => result.toString shouldBe text
      case Failure(_, message) => fail(message)
    }
  }

  it should "parse a json value" in {
    val text = """{"hello":"world"}"""

    service.parseExpression(text) match {
      case Success(result) => result.toString shouldBe text
      case Failure(_, message) => fail(message)
    }
  }

  it should "parse with parenthesis too" in {
    val text = """(var 1 > var 2) && (var 3 < var 1)"""

    service.parseExpression(text) match {
      case Success(result) => result.toString shouldBe text
      case Failure(_, message) => fail(message)
    }
  }

  it should "parse more elements" in {
    val text = """var 1 + var 2 - 34 > var 1 - var2 && var 1 = 34"""

    service.parseExpression(text) match {
      case Success(result) => result.toString shouldBe text
      case Failure(_, message) => fail(message)
    }
  }

  it should "parse boolean with only variables" in {
    val text = """var 1 && var 2"""

    service.parseExpression(text) match {
      case Success(result:BooleanExpression) =>
        result.toString shouldBe text
        result.op shouldBe And
      case Failure(_, message) => fail(message)
    }
  }

  it should "use operator precedence to parse the expression" in {
    val text = "contract = signed && Variable 2 > 10"

    service.parseExpression(text) match {
      case Success(BooleanExpression(_:ComparisonExpression, _:ComparisonExpression, And)) =>
      case Success(ComparisonExpression(left, right, op)) =>
        println(text)
        fail(s"left:$left, leftType ${left.getClass.getSimpleName}, right:$right, rightType ${right.getClass.getSimpleName} op:$op")
      case Success(other) =>
        fail(s"expression should be a boolean expression, instead it is ${other.getClass.getSimpleName} ${other}")
      case Failure(_, message) => fail(message)
    }
  }

  it should "parse that correctly" in {
    val text = "fill in the draft.state = \"done\" && (Variable 2 > 10)"

    service.parseExpression(text) match {
      case Success(BooleanExpression(_:ComparisonExpression, ParensExpression(_:ComparisonExpression), And)) =>
      case Success(other) =>
        fail(s"expression should be a boolean expression, instead it is ${other.getClass.getSimpleName} ${other}")
      case Failure(_, message) => fail(message)
    }
  }

  it should "have boolean literal true" in {
    val text = "fill in the draft.isDone = true"

    service.parseExpression(text) match {
      case Success(ComparisonExpression(_, BooleanConstant(true,_), Equals)) =>
      case Success(ComparisonExpression(left, right, Equals)) =>
        fail(s"left expression is of type ${left.getClass.getSimpleName} right expression ${right.getClass.getSimpleName}")
      case Success(other) =>
        fail(s"expression should be a Comparison, instead it is ${other.getClass.getSimpleName} ${other}")
      case Failure(_, message) => fail(message)
    }
  }

  it should "have boolean literal false" in {
    val text = "fill in the draft.isDone = false"

    service.parseExpression(text) match {
      case Success(ComparisonExpression(_, BooleanConstant(false,_), Equals)) =>
      case Success(other) =>
        fail(s"expression should be a boolean expression, instead it is ${other.getClass.getSimpleName} ${other}")
      case Failure(_, message) => fail(message)
    }
  }

  it should "not have a clash between boolean literal and variable names" in {
    val text = "falseVariable = false"

    service.parseExpression(text) match {
      case Success(ComparisonExpression(_, BooleanConstant(false,_), Equals)) =>
      case Success(other) =>
        fail(s"expression should be a boolean expression, instead it is ${other.getClass.getSimpleName} ${other}")
      case Failure(_, message) => fail(message)
    }
  }

  it should "be able to use boolean constants on its own" in {
    val text = "true"

    service.parseExpression(text) match {
      case Success(BooleanConstant(true,_)) =>
      case Success(other) =>
        fail(s"expression should be a boolean expression, instead it is ${other.getClass.getSimpleName} ${other}")
      case Failure(_, message) => fail(message)
    }
  }
}
