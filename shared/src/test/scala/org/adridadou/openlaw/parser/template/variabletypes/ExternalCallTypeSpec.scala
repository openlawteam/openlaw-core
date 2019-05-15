package org.adridadou.openlaw.parser.template.variabletypes

import org.adridadou.openlaw.parser.template.variableTypes.{IntegratedServiceDefinition, NumberType, ServiceName}
import org.scalatest.{FlatSpec, Matchers}
import io.circe.parser._
import io.circe.syntax._
import org.adridadou.openlaw.parser.template.{VariableDefinition, VariableName, VariableTypeDefinition}
import org.adridadou.openlaw.result.Success

class ExternalCallTypeSpec extends FlatSpec with Matchers {

  "ServiceName" should "be decoded from json" in {
    val json =
      """
        |{
        | "serviceName": "this is a test"
        |}
      """.stripMargin

    decode[ServiceName](json) match {
      case Right(service) => service.serviceName shouldBe "this is a test"
      case Left(err) => fail(err)
    }
  }

  "IntegratedServiceDefinition" should "be decoded from json" in {
    val Success(definition) = IntegratedServiceDefinition(
      """
        |[[Input:Structure(numberA: Number;
        |numberB: Number)]]
        |
        |[[Output:Structure(result: Number)]]
      """.stripMargin)

    val json = definition.asJson.noSpaces

    decode[IntegratedServiceDefinition](json) match {
      case Right(isd) =>
        isd.input.typeDefinition shouldBe Map(VariableName("numberA") -> VariableDefinition("numberA", NumberType), VariableName("numberB") -> VariableDefinition("numberB", NumberType))
        isd.output.typeDefinition shouldBe Map(VariableName("result") -> VariableDefinition("result", NumberType))
      case Left(err) =>
        fail(err)
    }
  }

}
