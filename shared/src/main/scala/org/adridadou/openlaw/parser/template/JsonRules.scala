package org.adridadou.openlaw.parser.template

/*
 * Copyright (C) 2009-2013 Mathias Doenitz, Alexander Myltsev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import io.circe.Json

import scala.annotation.switch
import org.parboiled2._
import CharPredicate.{Digit, Digit19, HexDigit}

/**
  * This is a feature-complete JSON parser implementation that almost directly
  * models the JSON grammar presented at http://www.json.org as a parboiled2 PEG parser.
  */
trait JsonRules extends GlobalRules with StringBuilding{

  // the root rule
  def jsonDefinition = rule {
      JsonObject |
      JsonArray
  }

  def JsonObject: Rule1[Json] = rule {
    ws2('{') ~ zeroOrMore(Pair).separatedBy(ws2(',')) ~ ws2('}') ~> ((fields: Seq[(String, Json)]) => Json.fromFields(fields))
  }

  def Pair = rule {
    JsonStringUnwrapped ~ ws2(':') ~ Value ~> ((_, _))
  }

  def Value: Rule1[Json] = rule {
    // as an optimization of the equivalent rule:
    //   JsonString | JsonNumber | JsonObject | JsonArray | JsonTrue | JsonFalse | JsonNull
    // we make use of the fact that one-char lookahead is enough to discriminate the cases
    run {
      (cursorChar: @switch) match {
        case '"' => JsonString
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' => JsonNumber
        case '{' => JsonObject
        case '[' => JsonArray
        case 't' => JsonTrue
        case 'f' => JsonFalse
        case 'n' => JsonNull
        case _ => MISMATCH
      }
    }
  }

  def JsonString = rule {
    JsonStringUnwrapped ~> Json.fromString _
  }

  def JsonStringUnwrapped = rule {
    '"' ~ clearSB() ~ Characters ~ ws2('"') ~ push(sb.toString)
  }

  def JsonNumber = rule {
    capture(Integer ~ optional(Frac) ~ optional(Exp)) ~> ((str) => Json.fromBigDecimal(BigDecimal(str))) ~ WhiteSpace
  }

  def JsonArray = rule {
    ws2('[') ~ zeroOrMore(Value).separatedBy(ws2(',')) ~ ws2(']') ~> (Json.arr(_: _*))
  }

  def Characters = rule {
    zeroOrMore(NormalChar | '\\' ~ EscapedChar)
  }

  def NormalChar = rule {
    !QuoteBackslash ~ ANY ~ appendSB()
  }

  def EscapedChar = rule(
    QuoteSlashBackSlash ~ appendSB()
      | 'b' ~ appendSB('\b')
      | 'f' ~ appendSB('\f')
      | 'n' ~ appendSB('\n')
      | 'r' ~ appendSB('\r')
      | 't' ~ appendSB('\t')
      | Unicode ~> { code => sb.append(code.toChar); () }
  )

  def Unicode = rule {
    'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16))
  }

  def Integer = rule {
    optional('-') ~ (Digit19 ~ Digits | Digit)
  }

  def Digits = rule {
    oneOrMore(Digit)
  }

  def Frac = rule {
    "." ~ Digits
  }

  def Exp = rule {
    ignoreCase('e') ~ optional(anyOf("+-")) ~ Digits
  }

  def JsonTrue = rule {
    "true" ~ WhiteSpace ~ push(Json.True)
  }

  def JsonFalse = rule {
    "false" ~ WhiteSpace ~ push(Json.False)
  }

  def JsonNull = rule {
    "null" ~ WhiteSpace ~ push(Json.Null)
  }

  def WhiteSpace = rule {
    zeroOrMore(WhiteSpaceChar)
  }

  def ws2(c: Char) = rule {
    c ~ WhiteSpace
  }

  val WhiteSpaceChar = CharPredicate(" \n\r\t\f")
  val QuoteBackslash = CharPredicate("\"\\")
  val QuoteSlashBackSlash = QuoteBackslash ++ "/"
}
