package org.adridadou.openlaw


import io.circe.Json
import org.adridadou.openlaw.values.TemplateTitle
import org.scalatest.check.Checkers
import org.scalatest.{FlatSpec, Matchers}
import io.circe.syntax._
import io.circe.parser._

class TemplateTitleSpec extends FlatSpec with Matchers with Checkers {
	val title = TemplateTitle("This is a Title")
	"Template title" should "serialize" in {
		title.asJson.noSpaces shouldBe "\"This is a Title\""
	}

	it should "deserialize plain string" in {
		val Right(dTitle) = decode[TemplateTitle](Json.fromString(title.originalTitle).noSpaces)

		dTitle shouldEqual title
	}

	it should "deserialize json titles as well" in {
		val json = "{\"title\":\"This is a Title\"}"

		decode[TemplateTitle](json) shouldEqual Right(title)
	}

}
