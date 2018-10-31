package org.adridadou.openlaw.parser.template

import org.adridadou.openlaw.parser.template.expressions.Expression
import org.parboiled2.ParseError
import scala.util.{Failure, Success}


/**
  * Created by davidroon on 05.06.17.
  */
class ExpressionParserService() {
  def parseExpression(source: String):Either[String, Expression] = {
    val compiler = createExpressionParser(source)

    compiler.root.run() match {
      case Failure(parseError:ParseError) => Left(compiler.formatError(parseError))
      case Failure(ex) => Left(ex.getMessage)
      case Success(result) => Right(result)
    }
  }

  private def createExpressionParser(expr:String):ExpressionParser = new ExpressionParser(expr)
}
