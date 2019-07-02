package org.adridadou.openlaw.parser.template.variabletypes

import java.time.{Clock, LocalDateTime}

import org.adridadou.openlaw.parser.template.variableTypes._
import org.scalatest.{FlatSpec, Matchers}
import io.circe.parser._
import io.circe.syntax._
import org.adridadou.openlaw.{OpenlawBigDecimal, oracles}
import org.adridadou.openlaw.oracles.{LoadTemplate, OpenlawSignatureOracle, UserId}
import org.adridadou.openlaw.parser.template.{ActionIdentifier, ExecutionFinished, ExpressionParserService, OpenlawTemplateLanguageParserService, VariableDefinition, VariableName, variableTypes}
import org.adridadou.openlaw.result.{Failure, Success}
import org.adridadou.openlaw.result.Implicits.RichResult
import org.adridadou.openlaw.values.{ContractDefinition, ContractId, TemplateId, TemplateParameters}
import org.adridadou.openlaw.vm.{ContractCreated, OpenlawVmProvider, TestAccount, TestCryptoService}
import play.api.libs.json.Json

class ExternalCallTypeSpec extends FlatSpec with Matchers {

  val parser:OpenlawTemplateLanguageParserService = new OpenlawTemplateLanguageParserService(Clock.systemUTC())
  val exprParser = new ExpressionParserService()
  val vmProvider:OpenlawVmProvider = new OpenlawVmProvider(TestCryptoService, parser)
  val clock: Clock = Clock.systemUTC()
  val serverAccount:TestAccount = TestAccount.newRandom

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
        |[[Output:Structure(sum: Number)]]
      """.stripMargin)

    val json = definition.asJson.noSpaces

    decode[IntegratedServiceDefinition](json) match {
      case Right(isd) =>
        isd.input.typeDefinition shouldBe Map(VariableName("numberA") -> VariableDefinition("numberA", NumberType), VariableName("numberB") -> VariableDefinition("numberB", NumberType))
        isd.output.typeDefinition shouldBe Map(VariableName("sum") -> VariableDefinition("sum", NumberType))
      case Left(err) =>
        fail(err)
    }
  }

  it should "access the parameters defined in the output abi" in {
    val templateContent =
      """<%
        |[[numberA:Number]]
        |[[numberB:Number]]
        |[[externalCall:ExternalCall(
        |serviceName: "Sum Service";
        |parameters:
        | numberA -> numberA,
        | numberB -> numberB;
        |startDate: '2018-12-12 00:00:00';
        |endDate: '2048-12-12 00:00:00')]]
        |%>
        |
        |[[identity:Identity]]
        |
        |[[externalCall.result.sum]]
      """.stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(templateContent))
    val email = Email("email@email.com").getOrThrow()
    val identity = Identity(email)
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters(
        "identity" -> IdentityType.internalFormat(identity).getOrThrow(),
        "numberA" -> NumberType.internalFormat(BigDecimal(2)).getOrThrow(),
        "numberB" -> NumberType.internalFormat(BigDecimal(2)).getOrThrow()
      )
    )

    val contractId = definition.id(TestCryptoService)
    val Success(abi) = variableTypes.IntegratedServiceDefinition(
      """[[Input:Structure(
        |numberA: Number;
        |numberB: Number
        |)]]
        |
        |[[Output:Structure(
        |sum:Number
        |)]]
        |""".stripMargin
    )

    val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq(), Map(ServiceName("Sum Service") -> abi))
    vm(LoadTemplate(templateContent))
    vm.executionResultState shouldBe ExecutionFinished
    vm.executionState shouldBe ContractCreated

    val signature = EthereumSignature(sign(identity, contractId).signature)
    val signatureEvent = oracles.OpenlawSignatureEvent(contractId, email, "", signature, EthereumHash.empty)
    vm(signatureEvent)

    val identifier = ActionIdentifier("Sum Service#numberA->2#numberB->2")
    val requestIdentifier = RequestIdentifier("test exec hash")

    val pendingExternalCallEvent = oracles.PendingExternalCallEvent(identifier, requestIdentifier, LocalDateTime.now)
    vm(pendingExternalCallEvent)

    val jsonResponse = Json.obj("sum" -> "4")

    val successfulExternalCallEvent = vm.executionResult match {
      case Some(executionResult) =>
        oracles.SuccessfulExternalCallEvent(identifier, requestIdentifier, LocalDateTime.now,
          abi.definedOutput.internalFormat(abi.definedOutput.cast(jsonResponse.toString, executionResult).getOrThrow()).getOrThrow())
      case None => fail("no execution result found!")
    }

    vm(successfulExternalCallEvent)
    vm.getAllExecutedVariables(ExternalCallType).getOrThrow().size shouldBe 1

    val execution = variableTypes.SuccessfulExternalCallExecution(LocalDateTime.now, LocalDateTime.now, jsonResponse.toString, requestIdentifier)

    val Some((exec, varDef)) = vm.newExecution(identifier, execution).getAllExecutedVariables(ExternalCallType).getOrThrow().headOption

    varDef.varType(exec).keysType(Seq("result", "sum"), varDef, exec) match {
      case Success(variableType) => variableType.name shouldBe "Number"
      case Failure(_, msg) => fail(msg)
    }

  }

  private def sign(identity: Identity, contractId: ContractId): EthereumSignature =
    signByEmail(identity.email, contractId.data)

  private def signByEmail(email:Email, data:EthereumData):EthereumSignature =
    EthereumSignature(serverAccount.sign(EthereumData(TestCryptoService.sha256(email.email))
      .merge(EthereumData(TestCryptoService.sha256(data.data)))).signature)

}
