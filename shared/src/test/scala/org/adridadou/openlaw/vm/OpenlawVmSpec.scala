package org.adridadou.openlaw.vm

import java.time.{Clock, LocalDateTime}

import org.adridadou.openlaw.oracles
import org.adridadou.openlaw.oracles._
import org.adridadou.openlaw.parser.template.{ExecutionFinished, OpenlawTemplateLanguageParserService, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.{ContractDefinition, ContractId, TemplateId, TemplateParameters}
import org.scalatest.{FlatSpec, Matchers}

class OpenlawVmSpec extends FlatSpec with Matchers {
  val parser:OpenlawTemplateLanguageParserService = new OpenlawTemplateLanguageParserService(Clock.systemUTC())
  val vmProvider:OpenlawVmProvider = new OpenlawVmProvider(TestCryptoService, parser)
  val clock: Clock = Clock.systemUTC()
  val serverAccount:TestAccount = TestAccount.newRandom

  "Openlaw Virtual Machine" should "be instantiated by giving a template, parameters and paragraph overrides" in {
    val definition = ContractDefinition(
      creatorId = UserId("dummyUser"),
      mainTemplate = TemplateId(),
      templates = Map(),
      parameters = TemplateParameters()
    )
    val vm = Option(vmProvider.create(definition, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq()))

    vm.isDefined shouldBe true
  }

  it should "being able to query the current state of the contract" in {
    val template = "this is a contract [[Hello:Text]]"
    val templateId = TemplateId(TestCryptoService.sha256(template))

    val definition = ContractDefinition(
      creatorId = UserId("dummyUser"),
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters(Map(VariableName("Hello") -> "World"))
    )

    val vm = vmProvider.create(definition, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())

    vm(LoadTemplate(template))

    vm.evaluate[String]("Hello") shouldBe Right("World")
    vm.evaluate[String]("Hello + ' World'") shouldBe Right("World World")
  }

  it should "be possible to register signature as incoming events and distinguish failed signatures" in {
    val template = "this is a contract [[Signatory:Identity]]"
    val identity1: Identity = Identity.withEmail(Email("hello@world.com"))
    val identity2: Identity = Identity.withEmail(Email("wrong@gmail.com"))
    val templateId = TemplateId(TestCryptoService.sha256(template))

    val definition1 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters(Map(VariableName("Signatory") -> IdentityType.internalFormat(identity1)))
    )
    val definition2 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters(Map(VariableName("Signatory") -> IdentityType.internalFormat(identity1)))
    )
    val vm1 = vmProvider.create(definition1, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    val vm2 = vmProvider.create(definition2, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    vm1(LoadTemplate(template))
    vm2(LoadTemplate(template))
    val result1 = sign(identity1, vm1.contractId)
    val signature1 = EthereumSignature(result1.signature)
    val signatureEvent = oracles.OpenlawSignatureEvent(definition1.id(TestCryptoService), identity1.email, "", signature1, EthereumHash.empty)

    val result2 = sign(identity1, vm2.contractId)
    val signature2 = EthereumSignature(result2.signature)


    vm1(signatureEvent)
    vm1(oracles.OpenlawSignatureEvent(definition2.id(TestCryptoService), identity1.email, "", signature2, EthereumHash.empty))
    vm1.signature(identity1.email) shouldBe Some(signatureEvent)
    vm1.signature(identity2.email) shouldBe None
  }

  it should "be able to update the state and reflect this in the vm" in {

    val template =
      """
        |[[My Contract Call:EthereumCall(
        |contract:"0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe";
        |interface:'ipfs:5ihruiherg34893zf';
        |network:"4";
        |startDate: '2018-12-12 00:00:00';
        |function:'callFunction';
        |arguments:'hello';
        |repeatEvery:'1 minute 12 seconds')]]
        |[[identity:Identity]]
        |[[identity2:Identity]]
      """.stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val email = Email("email@email.com")
    val userId = UserId("userId")

    val email2 = Email("email2@email.com")
    val userId2 = UserId("userId2")

    val identity = Identity(email)
    val identity2 = Identity(email2)
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters(
        "identity" -> IdentityType.internalFormat(identity),
        "identity2" -> IdentityType.internalFormat(identity2)
      )
    )

    val contractId = definition.id(TestCryptoService)

    val vm = vmProvider.create(definition, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    vm(LoadTemplate(template))
    vm.executionResultState shouldBe ExecutionFinished

    vm.executionState shouldBe ContractCreated
    val signature = EthereumSignature(sign(identity, contractId).signature)
    val signatureEvent = oracles.OpenlawSignatureEvent(contractId, email, "", signature, EthereumHash.empty)

    vm(signatureEvent)

    vm.executionState shouldBe ContractCreated

    val signature2 = EthereumSignature(sign(identity2, contractId).signature)
    val signatureEvent2 = oracles.OpenlawSignatureEvent(contractId, email2, "", signature2, EthereumHash.empty)

    vm(signatureEvent2)

    vm.executionState shouldBe ContractRunning

    val startDate = LocalDateTime.now(clock)
      .withYear(2018).withMonth(12).withDayOfMonth(12).withHour(0).withMinute(0).withSecond(0).withNano(0)

    vm.nextActionSchedule shouldBe Some(startDate)
    val execution = EthereumSmartContractExecution(startDate, startDate, SuccessfulExecution, EthereumHash(Array[Byte]()))
    vm.newExecution(VariableName("My Contract Call"), execution)

    vm.nextActionSchedule shouldBe Some(LocalDateTime
      .now(clock)
      .withYear(2018).withMonth(12).withDayOfMonth(12).withHour(0).withMinute(1).withSecond(12).withNano(0)
    )

    vm.newExecution(VariableName("My Contract Call"), EthereumSmartContractExecution(
      LocalDateTime.now(), LocalDateTime.now(), FailedExecution, EthereumHash.empty
    ))

    vm.executionState shouldBe ContractStopped
  }

  it should "be possible to register stopped contract, distinguish failed stops, and resume a stopped contract" in {
    val template = "this is a contract [[Signatory:Identity]]"
    val identity1: Identity = Identity.withEmail(Email("hello@world.com"))
    val identity2: Identity = Identity.withEmail(Email("wrong@gmail.com"))
    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition1 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters("Signatory" -> IdentityType.internalFormat(identity1))
    )
    val definition2 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters("Signatory" -> IdentityType.internalFormat(identity2))
    )
    val vm1 = vmProvider.create(definition1, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq(StopContractOracle(TestCryptoService), ResumeContractOracle(TestCryptoService)))
    val vm2 = vmProvider.create(definition2, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq(StopContractOracle(TestCryptoService), ResumeContractOracle(TestCryptoService)))

    vm1(LoadTemplate(template))
    vm2(LoadTemplate(template))

    val contractId1 = vm1.contractId
    val contractId2 = vm2.contractId

    vm1.executionState shouldBe ContractCreated

    val signature = sign(identity1, contractId1)
    val signatureFromNoStopping1 = EthereumSignature(signature.signature)
    val signatureEvent = oracles.OpenlawSignatureEvent(definition1.id(TestCryptoService), identity1.email, "", signatureFromNoStopping1, EthereumHash.empty)
    vm1(signatureEvent)
    vm1.executionState shouldBe ContractRunning

    val signature2 = sign(identity2, contractId2)
    val signatureFromNoStopping2 = EthereumSignature(signature2.signature)
    val signatureEvent2 = oracles.OpenlawSignatureEvent(definition2.id(TestCryptoService), identity2.email, "", signatureFromNoStopping2, EthereumHash.empty)
    vm2(signatureEvent2)
    vm2.executionState shouldBe ContractRunning

    val result1 = signForStopping(identity1, contractId1)
    val signatureFromStopping1 = EthereumSignature(result1.signature)

    val result2 = signForStopping(identity2, contractId2)
    val signatureFromStopping2 = EthereumSignature(result2.signature)

    vm2(StopExecutionEvent(signatureFromStopping1))
    vm2.executionState shouldBe ContractRunning
    vm2(StopExecutionEvent(signatureFromStopping2))
    vm2.executionState shouldBe ContractStopped
    vm1.executionState shouldBe ContractRunning

    vm1(StopExecutionEvent(signatureFromStopping1))
    vm1.executionState shouldBe ContractStopped
    vm1.signature(identity1.email) shouldBe Some(signatureEvent)
    vm1.signature(identity2.email) shouldBe None

    val result3 = signForResuming(identity2, contractId2)
    val signatureForResuming2 = EthereumSignature(result3.signature)

    vm2(ResumeExecutionEvent(signatureForResuming2))
    vm2.executionState shouldBe ContractResumed
    vm1(ResumeExecutionEvent(signatureForResuming2))
    vm1.executionState shouldBe ContractStopped

  }

  def sign(identity: Identity, contractId: ContractId): EthereumSignature = {
    signByEmail(identity.email, contractId.data)
  }

  def signForStopping(identity: Identity, contractId: ContractId): EthereumSignature = {
    signByEmail(identity.email, contractId.stopContract(TestCryptoService))
  }

  def signForResuming(identity:Identity, contractId: ContractId): EthereumSignature = {
    signByEmail(identity.email, contractId.resumeContract(TestCryptoService))
  }

  private def signByEmail(email:Email, data:EthereumData):EthereumSignature =
    EthereumSignature(serverAccount.sign(EthereumData(TestCryptoService.sha256(email.email))
      .merge(EthereumData(TestCryptoService.sha256(data.data)))).signature)
}
