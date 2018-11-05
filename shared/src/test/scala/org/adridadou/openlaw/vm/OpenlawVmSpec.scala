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

  "Openlaw Virtual Machine" should "be instantiated by giving a template, parameters and paragraph overrides" in {
    val definition = ContractDefinition(
      creatorId = UserId("dummyUser"),
      mainTemplate = TemplateId(),
      templates = Map(),
      parameters = TemplateParameters()
    )
    val vm = Option(vmProvider.create(definition, Seq(), Seq()))

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

    val vm = vmProvider.create(definition, Seq(), Seq())

    vm(LoadTemplate(template))

    vm.evaluate[String]("Hello") shouldBe Right("World")
    vm.evaluate[String]("Hello + ' World'") shouldBe Right("World World")
  }

  it should "be possible to register signature as incoming events and distinguish failed signatures" in {
    val template = "this is a contract [[Signatory:Identity]]"
    val identity1: Identity = Identity.withEmail(Email("hello@world.com")).withId(UserId.generateNew)
    val identity2: Identity = Identity.withEmail(Email("wrong@gmail.com")).withId(UserId.generateNew)
    val templateId = TemplateId(TestCryptoService.sha256(template))

    val definition1 = ContractDefinition(
      creatorId = identity1.userId,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters(Map(VariableName("Signatory") -> IdentityType.internalFormat(identity1)))
    )
    val definition2 = ContractDefinition(
      creatorId = identity2.userId,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters(Map(VariableName("Signatory") -> IdentityType.internalFormat(identity1)))
    )
    val vm1 = vmProvider.create(definition1, Seq(OpenlawSignatureOracle(TestCryptoService)), Seq())
    val vm2 = vmProvider.create(definition2, Seq(OpenlawSignatureOracle(TestCryptoService)), Seq())
    vm1(LoadTemplate(template))
    vm2(LoadTemplate(template))
    val result1 = sign(identity1.userId, vm1.contractId)
    val address1 = result1.address
    val signature1 = result1.signature
    val signatureEvent = oracles.OpenlawSignatureEvent(definition1.id(TestCryptoService), identity1.userId, identity1.email, "", address1, signature1, EthereumHash.empty)

    val result2 = sign(identity1.userId, vm2.contractId)
    val signature2 = result2.signature


    vm1(signatureEvent)
    vm1(oracles.OpenlawSignatureEvent(definition2.id(TestCryptoService), identity1.userId, identity1.email, "", address1, signature2, EthereumHash.empty))
    vm1.signature(identity1.userId) shouldBe Some(signatureEvent)
    vm1.signature(identity2.userId) shouldBe None
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
      """.stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))

    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters()
    )

    val vm = vmProvider.create(definition, Seq(OpenlawSignatureOracle(TestCryptoService)), Seq())
    vm(LoadTemplate(template))
    vm.executionResultState shouldBe ExecutionFinished

    vm(UpdateExecutionStateCommand(ContractRunning, LoadTemplate(template)))

    val startDate = LocalDateTime.now(clock)
      .withYear(2018).withMonth(12).withDayOfMonth(12).withHour(0).withMinute(0).withSecond(0).withNano(0)

    vm.nextActionSchedule shouldBe Some(startDate)
    val execution = EthereumSmartContractExecution(startDate, startDate, SuccessfulExecution, EthereumHash(Array[Byte]()))
    vm.newExecution(VariableName("My Contract Call"), execution)

    vm.nextActionSchedule shouldBe Some(LocalDateTime
      .now(clock)
      .withYear(2018).withMonth(12).withDayOfMonth(12).withHour(0).withMinute(1).withSecond(12).withNano(0)
    )
  }

  it should "be possible to register stopped contract, distinguish failed stops, and resume a stopped contract" in {
    val template = "this is a contract [[Signatory:Identity]]"
    val identity1: Identity = Identity.withEmail(Email("hello@world.com")).withId(UserId.generateNew)
    val identity2: Identity = Identity.withEmail(Email("wrong@gmail.com")).withId(UserId.generateNew)
    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition1 = ContractDefinition(
      creatorId = identity1.userId,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters("Signatory" -> IdentityType.internalFormat(identity1))
    )
    val definition2 = ContractDefinition(
      creatorId = identity2.userId,
      mainTemplate = templateId,
      templates = Map(),
      parameters = TemplateParameters("Signatory" -> IdentityType.internalFormat(identity2))
    )
    val vm1 = vmProvider.create(definition1, Seq(OpenlawSignatureOracle(TestCryptoService)), Seq(StopContractOracle(TestCryptoService), ResumeContractOracle(TestCryptoService)))
    val vm2 = vmProvider.create(definition2, Seq(OpenlawSignatureOracle(TestCryptoService)), Seq(StopContractOracle(TestCryptoService), ResumeContractOracle(TestCryptoService)))

    vm1(LoadTemplate(template))
    vm2(LoadTemplate(template))

    val contractId1 = vm1.contractId
    val contractId2 = vm2.contractId

    vm1.executionState shouldBe ContractCreated

    val signature = sign(identity1.userId, contractId1)
    val addressFromNoStopping1 = signature.address
    val signatureFromNoStopping1 = signature.signature
    val signatureEvent = oracles.OpenlawSignatureEvent(definition1.id(TestCryptoService), identity1.userId, identity1.email, "", addressFromNoStopping1, signatureFromNoStopping1, EthereumHash.empty)
    vm1(signatureEvent)
    vm1.executionState shouldBe ContractRunning

    val signature2 = sign(identity2.userId, contractId2)
    val addressFromNoStopping2 = signature2.address
    val signatureFromNoStopping2 = signature2.signature
    val signatureEvent2 = oracles.OpenlawSignatureEvent(definition2.id(TestCryptoService), identity2.userId, identity2.email, "", addressFromNoStopping2, signatureFromNoStopping2, EthereumHash.empty)
    vm2(signatureEvent2)
    vm2.executionState shouldBe ContractRunning

    val result1 = signForStopping(identity1.userId, contractId1)
    val addressFromStopping1 = result1.address
    val signatureFromStopping1 = result1.signature

    val result2 = signForStopping(identity2.userId, contractId2)
    val addressFromStopping2 = result2.address
    val signatureFromStopping2 = result2.signature

    vm2(StopExecutionEvent(identity2.userId, "", addressFromStopping1, signatureFromStopping1))
    vm2.executionState shouldBe ContractRunning
    vm2(StopExecutionEvent(identity2.userId, "", addressFromStopping2, signatureFromStopping2))
    vm2.executionState shouldBe ContractStopped
    vm1.executionState shouldBe ContractRunning

    vm1(StopExecutionEvent(identity1.userId, "", addressFromStopping1, signatureFromStopping1))
    vm1.executionState shouldBe ContractStopped
    vm1.signature(identity1.id.getOrElse(UserId.generateNew)) shouldBe Some(signatureEvent)
    vm1.signature(identity2.id.getOrElse(UserId.generateNew)) shouldBe None

    val result3 = signForResuming(identity2.userId, contractId2)
    val addressForResuming2 = result3.address
    val signatureForResuming2 = result3.signature

    vm2(ResumeExecutionEvent(identity2.userId, "", addressForResuming2, signatureForResuming2))
    vm2.executionState shouldBe ContractResumed
    vm1(ResumeExecutionEvent(identity1.userId, "", addressForResuming2, signatureForResuming2))
    vm1.executionState shouldBe ContractStopped

  }

  private def sign(userId: UserId, contractId: ContractId): SignatureResult = {
    val account = TestAccount.newRandom
    val signature = account.sign(EthereumData(contractId.id))
    val address = account.address
    SignatureResult(userId, address, signature)
  }

  private def signForStopping(userId: UserId, contractId: ContractId): SignatureResult = {
    val account = TestAccount.newRandom
    val signature = account.sign(contractId.stopContract(TestCryptoService).data)
    val address = account.address
    SignatureResult(userId, address, signature)
  }

  private def signForResuming(userId: UserId, contractId: ContractId): SignatureResult = {
    val account = TestAccount.newRandom
    val data = contractId.resumeContract(TestCryptoService)
    val signature = account.sign(data)
    val address = account.address
    SignatureResult(userId, address, signature)
  }
}
