package org.adridadou.openlaw.vm

import java.time.{Clock, LocalDateTime}

import org.adridadou.openlaw.{OpenlawString, oracles}
import org.adridadou.openlaw.oracles._
import org.adridadou.openlaw.parser.template.{ExecutionFinished, ExpressionParserService, OpenlawTemplateLanguageParserService, VariableName}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Failure, Success}
import org.adridadou.openlaw.values.{ContractDefinition, ContractId, TemplateId, TemplateParameters}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._
import org.scalatest.EitherValues._

class OpenlawVmSpec extends FlatSpec with Matchers {
  val parser:OpenlawTemplateLanguageParserService = new OpenlawTemplateLanguageParserService(Clock.systemUTC())
  val exprParser = new ExpressionParserService()
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
    val vm = Option(vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq()))

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

    val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())

    vm(LoadTemplate(template))

    vm.evaluate[OpenlawString]("Hello").right.value.underlying shouldBe ("World")
    vm.evaluate[OpenlawString]("Hello + ' World'").right.value.underlying shouldBe ("World World")
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
    val vm1 = vmProvider.create(definition1, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    val vm2 = vmProvider.create(definition2, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
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
      """<%
        |[[My Contract Call:EthereumCall(
        |contract:"0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe";
        |interface:'ipfs:5ihruiherg34893zf';
        |startDate: '2018-12-12 00:00:00';
        |function:'callFunction';
        |arguments:'hello';
        |repeatEvery:'1 minute 12 seconds')]]
        |
        |[[My other call:EthereumCall(
        |contract:"0xde0B295669a9FD93d5F28D9Ec85E40f4fff97BAe";
        |interface:'ipfs:5ihruiherg34893zf';
        |startDate: '2019-12-12 00:00:00';
        |function:'callFunction';
        |arguments:'hello';
        |repeatEvery:'1 minute 12 seconds')]]
        |%>
        |{{My Contract Call.isSuccessful =>
        |hello world
        |[[My other call]]
        |}}
        |
        |[[identity:Identity]]
        |[[identity2:Identity]]
      """.stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val email = Email("email@email.com")
    val email2 = Email("email2@email.com")
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

    val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
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

    vm.allNextActions.size shouldBe 1

    vm.getAllExecutedVariables(EthereumCallType).size shouldBe 1

    vm.nextActionSchedule shouldBe Some(startDate)
    val execution = EthereumSmartContractExecution(startDate, startDate, SuccessfulExecution, EthereumHash.empty)
    vm.newExecution(VariableName("My Contract Call"), execution)

    vm.nextActionSchedule shouldBe Some(LocalDateTime
      .now(clock)
      .withYear(2018).withMonth(12).withDayOfMonth(12).withHour(0).withMinute(1).withSecond(12).withNano(0)
    )

    vm.allNextActions.size shouldBe 2
    vm.getAllExecutedVariables(EthereumCallType).size shouldBe 2

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
    val vm1 = vmProvider.create(definition1, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq(StopContractOracle(TestCryptoService), ResumeContractOracle(TestCryptoService)))
    val vm2 = vmProvider.create(definition2, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq(StopContractOracle(TestCryptoService), ResumeContractOracle(TestCryptoService)))

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

  it should "be able to send events to an event listener" in {

    val abi = """[{"constant":false,"inputs":[{"name":"value","type":"uint256"}],"name":"OpenlawSignatureEvent","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[],"name":"registerTokenLaunch","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":true,"name":"to","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"owner","type":"address"},{"indexed":true,"name":"spender","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Approval","type":"event"}]"""
    val template = s"""
        |[[Signature: EthereumEventFilter(
        |contract address: "0x531e0957391dabf46f8a9609d799ffd067bdbbc0";
        |interface: $abi;
        |event type name: "OpenlawSignatureEvent";
        |conditional filter: this.value = 2939)]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(creatorId = UserId("hello@world.com"), mainTemplate = templateId, templates = Map(), parameters = TemplateParameters())

    val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    vm(LoadTemplate(template))

    val values = Map(VariableName("owner") -> EthAddressType.internalFormat(EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0")), VariableName("value") -> NumberType.internalFormat(BigDecimal(2939)))
    val event = EthereumEventFilterEvent(VariableName("Signature"), EthereumHash.empty, EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0"), "OpenlawSignatureEvent", values, LocalDateTime.now)

    val ethereumEventFilterOracle = EthereumEventFilterOracle(parser)
    ethereumEventFilterOracle.incoming(vm, event) match {
      case Right(vm) =>
        vm.allExecutions.keys should contain(VariableName("Signature"))
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "be able to send an event to an event listener and use it in the template" in {

    val abi = """[{"constant":false,"inputs":[{"name":"value","type":"uint256"}],"name":"OpenlawSignatureEvent","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[],"name":"registerTokenLaunch","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":true,"name":"to","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"owner","type":"address"},{"indexed":true,"name":"spender","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Approval","type":"event"}]"""
    val template = s"""
        |[[Signature: EthereumEventFilter(
        |contract address: "0x531e0957391dabf46f8a9609d799ffd067bdbbc0";
        |interface: $abi;
        |event type name: "OpenlawSignatureEvent";
        |conditional filter: this.value = 2939)]]
        |
        |[[Signature.event.value]] [[Signature.received]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(creatorId = UserId("hello@world.com"), mainTemplate = templateId, templates = Map(), parameters = TemplateParameters())

    val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    vm(LoadTemplate(template))

    val values = Map(VariableName("owner") -> EthAddressType.internalFormat(EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0")), VariableName("value") -> NumberType.internalFormat(BigDecimal(2939)))
    val event = EthereumEventFilterEvent(VariableName("Signature"), EthereumHash.empty, EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0"), "OpenlawSignatureEvent", values, LocalDateTime.now)

    val ethereumEventFilterOracle = EthereumEventFilterOracle(parser)
    ethereumEventFilterOracle.incoming(vm, event) match {
      case Right(newVm) =>
        newVm.allExecutions.keys should contain(VariableName("Signature"))
        val text = parser.forReview(newVm.executionResult.value.agreements.head)
        text shouldBe "<p class=\"no-section\"><br /></p><p class=\"no-section\">2,939 true</p>"
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "make general info about the contract available" in {
    val template =
      """
        |[[info:OLInfo]]
        |
        |hello [[info.id]]. Your address is [[info.profileAddress]].
      """.stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(creatorId = UserId("hello@world.com"), mainTemplate = templateId, templates = Map(), parameters = TemplateParameters())

    val vm = vmProvider.create(definition, Some(EthereumAddress("0x531e0957391daff46f8a9609d799ffd067bdbbc0")), OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    vm(LoadTemplate(template))

    vm.executionResult match {
      case Some(executionResult) =>
        parser.forReview(executionResult.agreements.head) shouldBe s"""<p class="no-section"><br /></p><p class="no-section">hello ${definition.id(TestCryptoService)}. Your address is 0x531e0957391daff46f8a9609d799ffd067bdbbc0.<br />      </p>"""
      case None => fail("no execution result found!")
    }
  }

  it should "add init execution if we sign with ERC-712" in {

    val address = EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0")
    val contractAddress = EthereumAddress("0x531e0957391dabf46f8a9609d799ffd067bdbbc0")
    val abi = """[{"constant":false,"inputs":[{"name":"signature","type":"bytes"}],"name":"callSignature","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"}]"""
    val template = s"""
                      |[[Signature: EthereumCall(
                      |contract: "${contractAddress.withLeading0x}";
                      |interface: $abi;
                      |function: "signature";
                      |from: "${address.withLeading0x}";
                      |Signature parameter: "signature")]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(creatorId = UserId("hello@world.com"), mainTemplate = templateId, templates = Map(), parameters = TemplateParameters())

    val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq(EthereumERC712Oracle(TestCryptoService)))

    (for {
      _ <- vm.applyEvent(LoadTemplate(template))
      _ <- vm.applyEvent(PreparedERC712SmartContractCallEvent(VariableName("Signature"), address, EthereumSignature("0x123456789"), contractAddress))
    } yield vm ) match {
      case Success(_) =>

        vm.initExecution[PreparedERC712SmartContractCallExecution](VariableName("Signature")) shouldBe Some(PreparedERC712SmartContractCallExecution(VariableName("Signature"),EthereumSignature("0x1234567809")))
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "not process events which do not match the filter" in {

    val abi = """[{"constant":false,"inputs":[{"name":"owner","type":"address"},{"name":"value","type":"uint256"}],"name":"OpenlawSignatureEvent","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[],"name":"registerTokenLaunch","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":true,"name":"to","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"owner","type":"address"},{"indexed":true,"name":"spender","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Approval","type":"event"}]"""
    val template = s"""
        |[[Signature: EthereumEventFilter(
        |contract address: "0x531e0957391dabf46f8a9609d799ffd067bdbbc0";
        |interface: $abi;
        |event type name: "OpenlawSignatureEvent";
        |conditional filter: this.value = 2939)]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(creatorId = UserId("hello@world.com"), mainTemplate = templateId, templates = Map(), parameters = TemplateParameters())

    val vm = vmProvider.create(definition, None, OpenlawSignatureOracle(TestCryptoService, serverAccount.address), Seq())
    vm(LoadTemplate(template))

    val values = Map(VariableName("owner") -> EthAddressType.internalFormat(EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0")), VariableName("value") -> NumberType.internalFormat(BigDecimal(1000)))
    val event = EthereumEventFilterEvent(VariableName("Signature"), EthereumHash.empty, EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0"), "OpenlawSignatureEvent", values, LocalDateTime.now)

    val ethereumEventFilterOracle = EthereumEventFilterOracle(parser)
    (for {
      result <- ethereumEventFilterOracle.incoming(vm, event)
    } yield result) match {
      case Right(vm) =>
        vm.allExecutions.keys shouldNot contain(VariableName("Signature"))
      case Failure(ex, message) =>
        fail(message, ex)
    }
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
