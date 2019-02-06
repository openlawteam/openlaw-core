package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.parboiled2.ParseError

/**
  * Created by davidroon on 05.06.17.
  */
class ExpressionParserService() {
  def parseExpression(source: String): Result[Expression] = {
    val compiler = createExpressionParser(source)

    compiler.root.run() match {
      case scala.util.Failure(parseError:ParseError) => Failure(compiler.formatError(parseError))
      case scala.util.Failure(ex) => Failure(ex.getMessage)
      case scala.util.Success(result) => Success(result)
    }
  }

  private def createExpressionParser(expr:String):ExpressionParser = new ExpressionParser(expr)
}
