package org.adridadou.openlaw.parser

import org.adridadou.openlaw.parser.template._
import org.scalatest._


/**
  * Created by davidroon on 05.05.17.
  */
class ExpressionParserSpec  extends FlatSpec with Matchers {

  private val service = new ExpressionParserService()

  it should "parse simple expression" in {
    val text = "variable 1+23"

    service.parseExpression(text) match {
       case Right(result) => result.toString shouldBe text
       case Left(error) => fail(error.e)
    }
  }

  it should "parse simple string expression" in {
    val text = """variable 1+"hello world""""

    service.parseExpression(text) match {
      case Right(result) => result.toString shouldBe text
      case Left(error) => fail(error.e)
    }
  }

  it should "parse simple expression 2" in {
    val text = "23+variable 1"

    service.parseExpression(text) match {
      case Right(result) => result.toString shouldBe text
      case Left(error) => fail(error.e)
    }
  }

  it should "parse a json value" in {
    val text = """{"hello":"world"}"""

    service.parseExpression(text) match {
      case Right(result) => result.toString shouldBe text
      case Left(error) => fail(error.e)
    }
  }

  it should "parse with parenthesis too" in {
    val text = """(var 1 > var 2)&&(var 3 < var 1)"""

    service.parseExpression(text) match {
      case Right(result) => result.toString shouldBe text
      case Left(error) => fail(error.e)
    }
  }
}
