package org.adridadou.openlaw.parser

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.{BooleanExpression, ComparisonExpression}
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

  it should "parse the expression correctly" in {
    val text = "contract = signed && Variable 2 > 10"

    service.parseExpression(text) match {
      case Success(BooleanExpression(left, right, boolOp)) =>
        left match {
          case ComparisonExpression(contract, signed, op) =>

        }
        succeed
      case Success(ComparisonExpression(left, right, op)) =>
        println(text)
        fail(s"left:${left}, leftType ${left.getClass.getSimpleName}, right:${right}, rightType ${right.getClass.getSimpleName} op:${op}")
      case Success(other) =>
        fail(s"expression should be a boolean expression, instead it is ${other.getClass.getSimpleName} ${other}")
      case Failure(_, message) => fail(message)
    }
  }
}
