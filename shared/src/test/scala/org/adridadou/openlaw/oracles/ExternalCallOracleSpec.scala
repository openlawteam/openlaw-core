package org.adridadou.openlaw.oracles

import java.time.{Clock, LocalDateTime}

import org.adridadou.openlaw.parser.template.variableTypes.{IntegratedServiceDefinition, RequestIdentifier, ServiceName}
import org.adridadou.openlaw.{OpenlawMap, OpenlawValue}
import org.adridadou.openlaw.parser.template.{ActionIdentifier, OpenlawTemplateLanguageParserService, VariableName, VariableTypeDefinition}
import org.adridadou.openlaw.result.{Failure, Success}
import org.adridadou.openlaw.result.Implicits.RichResult
import org.adridadou.openlaw.values.{ContractDefinition, TemplateId, TemplateParameters}
import org.adridadou.openlaw.vm.{OpenlawExecutionEngine, OpenlawVmProvider, TestAccount, TestCryptoService}
import org.scalatest._
import org.scalatestplus.scalacheck.Checkers

import scala.util.Random

class ExternalCallOracleSpec extends FlatSpec with Matchers with Checkers {
  private val clock = Clock.systemUTC()
  private val parser:OpenlawTemplateLanguageParserService = new OpenlawTemplateLanguageParserService(clock)
  private val engine = new OpenlawExecutionEngine()
  private val vmProvider: OpenlawVmProvider = new OpenlawVmProvider(TestCryptoService, new OpenlawTemplateLanguageParserService(Clock.systemUTC()))
  private val serverAccount: TestAccount = TestAccount.newRandom
  private val oracle: ExternalCallOracle = ExternalCallOracle(TestCryptoService)

  private val structureTemplate =
    """
      |[[MyCallType:Structure(
      |computationResult: Text
      |)]]
    """.stripMargin

  private val Success(executionResult) = parser.compileTemplate(structureTemplate, clock)
    .flatMap(t => engine.execute(t))

  private val Some(structure) = executionResult.findVariableType(VariableTypeDefinition("MyCallType"))

  private val requestIdentifier = RequestIdentifier(Random.nextString(10))

  private val structureValues:OpenlawMap[VariableName, OpenlawValue] = OpenlawMap(Map(VariableName("computationResult") -> "value"))

  private val failExecutionCallEvent = FailedExternalCallEvent(ActionIdentifier("failedExternalCall"),
    requestIdentifier, LocalDateTime.parse("2018-12-12T00:00:00"), LocalDateTime.parse("2018-12-12T00:10:00"), "some error")
  private val pendingExecutionCallEvent = PendingExternalCallEvent(ActionIdentifier("pendingExternalCall"),
    requestIdentifier, LocalDateTime.parse("2018-12-12T00:00:00"))
  private val successExecutionCallEvent = SuccessfulExternalCallEvent(ActionIdentifier("successExternalCall"),
    requestIdentifier, LocalDateTime.parse("2018-12-12T00:00:00"), structure.internalFormat(structureValues).getOrThrow())

  private val template =
    """
      |[[param1:Text]]
      |[[param2:Text]]
      |[[externalCall:ExternalCall(
      |serviceName: "SomeIntegratedService";
      |args:
      |param1 -> param1,
      |param2 -> param2;
      |startDate: '2018-12-12 00:00:00';
      |endDate: '2048-12-12 00:00:00';
      |repeatEvery: '1 hour 30 minute')]]
      |[[externalCall]]
    """.stripMargin

  private val templateId = TemplateId(TestCryptoService.sha256(template))
  private val definition = ContractDefinition(UserId("hello@world.com"), templateId, Map(), TemplateParameters("param1" -> "a", "param2" -> "b"))
  private val externalCallStructures = Map(ServiceName("SomeIntegratedService") -> IntegratedServiceDefinition("""[[Input:Structure(param1:Text;param2:Text)]] [[Output:Structure(result:Text)]]""").getOrThrow())
  private val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq(oracle), externalCallStructures)

  vm(LoadTemplate(template))

  "ExternalCallOracle" should "execute external call events only" in {
    oracle.shouldExecute(failExecutionCallEvent) shouldBe true
    oracle.shouldExecute(pendingExecutionCallEvent) shouldBe true
    oracle.shouldExecute(successExecutionCallEvent) shouldBe true
  }

  it should "handle a failed execution call event" in {
    oracle.incoming(vm, failExecutionCallEvent) match {
      case Right(newVm) =>
        newVm.allExecutions.keys should contain(ActionIdentifier("failedExternalCall"))
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

}
