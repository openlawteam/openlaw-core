package org.adridadou.openlaw

import io.circe.Json
import org.adridadou.openlaw.values.TemplateTitle
import org.scalatest._
import flatspec._
import matchers._
import io.circe.syntax._
import io.circe.parser._

class PackageSpec extends AnyFlatSpec with should.Matchers {

  "An OpenlawValue instance" should "be equal" in {
    OpenlawString("test") should be(OpenlawString("test"))
    OpenlawString("test") shouldNot be(OpenlawString("TEST"))
    OpenlawBigDecimal(BigDecimal(1L)) should be(
      OpenlawBigDecimal(BigDecimal(1L))
    )
    OpenlawBigDecimal(BigDecimal(1L)) shouldNot be(
      OpenlawBigDecimal(BigDecimal(2L))
    )
  }

  it should "be comparable" in {
    OpenlawString("test").compareTo(OpenlawString("test")) should be(0)
    OpenlawString("test").compareTo(OpenlawString("TEST")) shouldNot be(0)
  }

  it should "implicitly convert to its underlying type" in {
    val string: String = OpenlawString("test")
    string should be("test")
    val bigDecimal: BigDecimal = OpenlawBigDecimal(BigDecimal(1L))
    bigDecimal should be(BigDecimal(1L))
  }
}
