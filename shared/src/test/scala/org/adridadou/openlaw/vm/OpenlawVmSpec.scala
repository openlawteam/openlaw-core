package org.adridadou.openlaw.vm

import java.time.{Instant, ZonedDateTime}

import org.adridadou.openlaw.{OpenlawMap, OpenlawString, oracles}
import org.adridadou.openlaw.oracles._
import org.adridadou.openlaw.parser.template.{
  ActionIdentifier,
  ExecutionFinished,
  ExpressionParserService,
  OpenlawTemplateLanguageParserService,
  VariableName,
  variableTypes
}
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.result.{Failure, Success}
import org.adridadou.openlaw.result.Implicits.RichResult
import org.adridadou.openlaw.values.{
  ContractDefinition,
  ContractId,
  TemplateId,
  TemplateParameters
}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._
import org.scalatest.EitherValues._

class OpenlawVmSpec extends FlatSpec with Matchers {
  val parser: OpenlawTemplateLanguageParserService =
    new OpenlawTemplateLanguageParserService()
  val exprParser = new ExpressionParserService()
  val vmProvider: OpenlawVmProvider =
    new OpenlawVmProvider(TestCryptoService, parser)
  val serverAccount: TestAccount = TestAccount.newRandom

  "Openlaw Virtual Machine" should "be instantiated by giving a template, parameters and paragraph overrides" in {
    val definition = ContractDefinition(
      creatorId = UserId("dummyUser"),
      mainTemplate = TemplateId(),
      templates = Map.empty,
      parameters = TemplateParameters()
    )
    val vm = Option(
      vmProvider.create(
        definition,
        None,
        OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
        Nil
      )
    )

    vm.isDefined shouldBe true
  }

  it should "being able to query the current state of the contract" in {
    val template = "this is a contract [[Hello:Text]]"
    val templateId = TemplateId(TestCryptoService.sha256(template))

    val definition = ContractDefinition(
      creatorId = UserId("dummyUser"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(Map(VariableName("Hello") -> "World"))
    )

    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )

    vm(LoadTemplate(template))

    vm.evaluate[OpenlawString]("Hello").right.value.underlying shouldBe "World"
    vm.evaluate[OpenlawString]("Hello + ' World'")
      .right
      .value
      .underlying shouldBe "World World"
  }

  it should "be possible to register signature as incoming events and distinguish failed signatures" in {
    val template = "this is a contract [[Signatory:Identity]]"
    val identity1: Identity =
      Identity.withEmail(Email("hello@world.com").right.value)
    val identity2: Identity =
      Identity.withEmail(Email("wrong@gmail.com").right.value)
    val templateId = TemplateId(TestCryptoService.sha256(template))

    val definition1 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        Map(
          VariableName("Signatory") -> IdentityType
            .internalFormat(identity1)
            .right
            .value
        )
      )
    )
    val definition2 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        Map(
          VariableName("Signatory") -> IdentityType
            .internalFormat(identity1)
            .right
            .value
        )
      )
    )
    val vm1 = vmProvider.create(
      definition1,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    val vm2 = vmProvider.create(
      definition2,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    vm1(LoadTemplate(template))
    vm2(LoadTemplate(template))
    val result1 = sign(identity1, vm1.contractId)
    val signature1 = EthereumSignature(result1.signature)
    val signatureEvent = oracles.OpenlawSignatureEvent(
      definition1.id(TestCryptoService),
      identity1.email,
      "",
      signature1
    )

    val result2 = sign(identity1, vm2.contractId)
    val signature2 = EthereumSignature(result2.signature)

    vm1(signatureEvent)
    vm1(
      oracles.OpenlawSignatureEvent(
        definition2.id(TestCryptoService),
        identity1.email,
        "",
        signature2
      )
    )
    vm1.signature(identity1.email) shouldBe Some(signatureEvent)
    vm1.signature(identity2.email) shouldBe None
  }

  it should "be possible to sign the contract with an external signature" in {
    val template =
      """this is a contract [[Signatory:ExternalSignature(service:"DocuSign")]]"""
    val Right(identity: Identity) =
      Email("hello@world.com").map(Identity.withEmail)
    val templateId = TemplateId(TestCryptoService.sha256(template))

    val serviceAccount = TestAccount.newRandom
    val serviceName = ServiceName("DocuSign")

    val definition = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        Map(
          VariableName("Signatory") -> ExternalSignatureType
            .internalFormat(
              ExternalSignature(
                serviceName = serviceName,
                identity = Some(identity)
              )
            )
            .right
            .value
        )
      )
    )

    val contractId = definition.id(TestCryptoService)

    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(
        TestCryptoService,
        serverAccount.address,
        Map(serviceName -> serviceAccount.address)
      ),
      Nil,
      Map(serviceName -> IntegratedServiceDefinition.signatureDefinition)
    )

    vm(LoadTemplate(template))
    vm.executionState shouldBe ContractCreated
    vm.allNextActions.map(_.map(_.action)) shouldBe Success(
      List(SignatureAction(identity.email, List(serviceName)))
    )

    val badSignature = EthereumSignature(
      signByEmail(identity.email, vm.contractId.data, serverAccount).signature
    )
    val signature = EthereumSignature(
      signByEmail(identity.email, vm.contractId.data, serviceAccount).signature
    )
    val badSignatureEvent = oracles.ExternalSignatureEvent(
      contractId,
      identity.email,
      "",
      serviceName,
      badSignature
    )
    val signatureEvent = oracles.ExternalSignatureEvent(
      contractId,
      identity.email,
      "",
      serviceName,
      signature
    )

    vm(badSignatureEvent)
    vm.signature(identity.email) shouldBe None
    vm(signatureEvent)
    vm.signature(identity.email).isDefined shouldBe true

    vm.executionState shouldBe ContractRunning
  }

  it should "be able to update the state and reflect this in the vm" in {

    val template =
      """<%
        |[[My Contract Call:EthereumCall(
        |contract:"0xde0B295669a9FD93d5F28D9Ec85E40f4cb697BAe";
        |interface:'ipfs:5ihruiherg34893zf';
        |startDate: '2018-12-12 00:00:00';
        |value: 2345678;
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
    val email = Email("email@email.com").right.value
    val email2 = Email("email2@email.com").right.value

    val identity = Identity(email)
    val identity2 = Identity(email2)
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        "identity" -> IdentityType.internalFormat(identity).right.value,
        "identity2" -> IdentityType.internalFormat(identity2).right.value
      )
    )

    val contractId = definition.id(TestCryptoService)

    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    val Success(_) = vm.applyEvent(LoadTemplate(template))
    vm.executionResultState shouldBe ExecutionFinished

    vm.executionState shouldBe ContractCreated
    val signature = EthereumSignature(sign(identity, contractId).signature)
    val signatureEvent =
      oracles.OpenlawSignatureEvent(contractId, email, "", signature)

    vm(signatureEvent)

    vm.executionState shouldBe ContractCreated

    val signature2 = EthereumSignature(sign(identity2, contractId).signature)
    val signatureEvent2 =
      oracles.OpenlawSignatureEvent(contractId, email2, "", signature2)

    vm(signatureEvent2)

    vm.executionState shouldBe ContractRunning

    val startDate = ZonedDateTime.now
      .withYear(2018)
      .withMonth(12)
      .withDayOfMonth(12)
      .withHour(0)
      .withMinute(0)
      .withSecond(0)
      .withNano(0)
      .toInstant

    vm.allNextActions.right.value.size shouldBe 1

    vm.getAllExecutedVariables(EthereumCallType).size shouldBe 1

    vm.nextActionSchedule.right.value shouldBe Some(startDate)

    val execution = EthereumSmartContractExecution(
      startDate,
      startDate,
      SuccessfulExecution,
      EthereumHash.empty
    )
    vm.allNextActions.right.value.size shouldBe 1

    val action = vm.allNextActions.right.value.head
    vm.getAllExecutedVariables(EthereumCallType).size shouldBe 1
    vm.newExecution(action.identifier.right.value, execution)

    vm.nextActionSchedule.getOrThrow() shouldBe Some(
      ZonedDateTime
        .now()
        .withYear(2018)
        .withMonth(12)
        .withDayOfMonth(12)
        .withHour(0)
        .withMinute(1)
        .withSecond(12)
        .withNano(0)
        .toInstant
    )

    vm.allNextActions.right.value.size shouldBe 2
    vm.getAllExecutedVariables(EthereumCallType).size shouldBe 2

    vm.newExecution(
      action.identifier.right.value,
      EthereumSmartContractExecution(
        Instant.now(),
        Instant.now(),
        FailedExecution,
        EthereumHash.empty
      )
    )

    vm.executionState shouldBe ContractStopped
  }

  it should "be possible to register stopped contract, distinguish failed stops, and resume a stopped contract" in {
    val template = "this is a contract [[Signatory:Identity]]"
    val identity1: Identity =
      Identity.withEmail(Email("hello@world.com").getOrThrow())
    val identity2: Identity =
      Identity.withEmail(Email("wrong@gmail.com").getOrThrow())
    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition1 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        "Signatory" -> IdentityType.internalFormat(identity1).getOrThrow()
      )
    )
    val definition2 = ContractDefinition(
      creatorId = UserId.SYSTEM_ID,
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        "Signatory" -> IdentityType.internalFormat(identity2).getOrThrow()
      )
    )
    val vm1 = vmProvider.create(
      definition1,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      List(
        StopContractOracle(TestCryptoService),
        ResumeContractOracle(TestCryptoService)
      )
    )
    val vm2 = vmProvider.create(
      definition2,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      List(
        StopContractOracle(TestCryptoService),
        ResumeContractOracle(TestCryptoService)
      )
    )

    vm1(LoadTemplate(template))
    vm2(LoadTemplate(template))

    val contractId1 = vm1.contractId
    val contractId2 = vm2.contractId

    vm1.executionState shouldBe ContractCreated

    val signature = sign(identity1, contractId1)
    val signatureFromNoStopping1 = EthereumSignature(signature.signature)
    val signatureEvent = oracles.OpenlawSignatureEvent(
      definition1.id(TestCryptoService),
      identity1.email,
      "",
      signatureFromNoStopping1
    )
    vm1(signatureEvent)
    vm1.executionState shouldBe ContractRunning

    val signature2 = sign(identity2, contractId2)
    val signatureFromNoStopping2 = EthereumSignature(signature2.signature)
    val signatureEvent2 = oracles.OpenlawSignatureEvent(
      definition2.id(TestCryptoService),
      identity2.email,
      "",
      signatureFromNoStopping2
    )
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

    val abi =
      """[{"constant":false,"inputs":[{"name":"value","type":"uint256"}],"name":"OpenlawSignatureEvent","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[],"name":"registerTokenLaunch","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":true,"name":"to","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"owner","type":"address"},{"indexed":true,"name":"spender","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Approval","type":"event"}]"""
    val template = s"""
        |[[Signature: EthereumEventFilter(
        |contract address: "0x531e0957391dabf46f8a9609d799ffd067bdbbc0";
        |interface: $abi;
        |event type name: "OpenlawSignatureEvent";
        |conditional filter: this.value = 2939)]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters()
    )

    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    vm(LoadTemplate(template))

    val values = Map(
      VariableName("owner") -> EthAddressType
        .internalFormat(
          EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0").right.value
        )
        .right
        .value,
      VariableName("value") -> NumberType
        .internalFormat(BigDecimal(2939))
        .right
        .value
    )
    val identifier = vm.executionResult
      .map(_.allActions.right.value)
      .getOrElse(Nil)
      .head
      .identifier
      .right
      .value
    val event = EthereumEventFilterEvent(
      identifier,
      EthereumHash.empty,
      EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0").right.value,
      "OpenlawSignatureEvent",
      values,
      Instant.now
    )

    val ethereumEventFilterOracle =
      EthereumEventFilterOracle(parser, TestCryptoService)
    ethereumEventFilterOracle.incoming(vm, event) match {
      case Right(vm) =>
        vm.allExecutions.keys should contain(identifier)
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "be able to send an event to an event listener and use it in the template" in {

    val abi =
      """[{"constant":false,"inputs":[{"name":"value","type":"uint256"}],"name":"OpenlawSignatureEvent","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[],"name":"registerTokenLaunch","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":true,"name":"to","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"owner","type":"address"},{"indexed":true,"name":"spender","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Approval","type":"event"}]"""
    val template = s"""
        |[[Signature: EthereumEventFilter(
        |contract address: "0x531e0957391dabf46f8a9609d799ffd067bdbbc0";
        |interface: $abi;
        |event type name: "OpenlawSignatureEvent";
        |conditional filter: this.value = 2939)]]
        |
        |[[Signature.event.value]] [[Signature.received]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters()
    )
    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    vm(LoadTemplate(template))

    val values = Map(
      VariableName("owner") -> EthAddressType
        .internalFormat(
          EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0").right.value
        )
        .right
        .value,
      VariableName("value") -> NumberType
        .internalFormat(BigDecimal(2939))
        .right
        .value
    )
    val identifier = vm.executionResult
      .map(_.allActions.right.value)
      .getOrElse(Nil)
      .head
      .identifier
      .right
      .value
    val event = EthereumEventFilterEvent(
      identifier,
      EthereumHash.empty,
      EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0").right.value,
      "OpenlawSignatureEvent",
      values,
      Instant.now
    )

    val ethereumEventFilterOracle =
      EthereumEventFilterOracle(parser, TestCryptoService)
    ethereumEventFilterOracle.incoming(vm, event) match {
      case Right(newVm) =>
        newVm.allExecutions.keys should contain(identifier)
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
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters()
    )

    val vm = vmProvider.create(
      definition,
      Some(
        EthereumAddress("0x531e0957391daff46f8a9609d799ffd067bdbbc0").right.value
      ),
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    vm(LoadTemplate(template))

    vm.executionResult match {
      case Some(executionResult) =>
        parser.forReview(executionResult.agreements.head) shouldBe s"""<p class="no-section"><br /></p><p class="no-section">hello ${definition
          .id(TestCryptoService)}. Your address is 0x531e0957391daff46f8a9609d799ffd067bdbbc0.<br />      </p>"""
      case None => fail("no execution result found!")
    }
  }

  it should "add init execution if we sign with ERC-712" in {

    val address =
      EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0").right.value
    val contractAddress =
      EthereumAddress("0x531e0957391dabf46f8a9609d799ffd067bdbbc0").right.value
    val abi =
      """[{"constant":false,"inputs":[{"name":"signature","type":"bytes"}],"name":"callSignature","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"}]"""
    val template = s"""
                      |[[Signature: EthereumCall(
                      |contract: "${contractAddress.withLeading0x}";
                      |interface: $abi;
                      |function: "signature";
                      |from: "${address.withLeading0x}";
                      |Signature parameter: "signature")]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters()
    )
    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      List(EthereumERC712Oracle(TestCryptoService))
    )

    (for {
      _ <- vm.applyEvent(LoadTemplate(template))
      _ <- vm.applyEvent(
        PreparedERC712SmartContractCallEvent(
          VariableName("Signature"),
          vm.executionResult
            .map(_.allActions.right.value)
            .getOrElse(Nil)
            .head
            .identifier
            .right
            .value,
          address,
          EthereumSignature("0x123456789").right.value,
          contractAddress
        )
      )
    } yield vm) match {
      case Success(_) =>
        vm.initExecution[PreparedERC712SmartContractCallExecution](
            vm.executionResult
              .map(_.allActions.right.value)
              .getOrElse(Nil)
              .head
              .identifier
              .right
              .value
          )
          .right
          .value shouldBe Some(
          PreparedERC712SmartContractCallExecution(
            vm.executionResult
              .map(_.allActions.right.value)
              .getOrElse(Seq())
              .head
              .identifier
              .right
              .value,
            EthereumSignature("0x1234567809").right.value
          )
        )
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "not process events which do not match the filter" in {

    val abi =
      """[{"constant":false,"inputs":[{"name":"owner","type":"address"},{"name":"value","type":"uint256"}],"name":"OpenlawSignatureEvent","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[],"name":"registerTokenLaunch","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"from","type":"address"},{"indexed":true,"name":"to","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"owner","type":"address"},{"indexed":true,"name":"spender","type":"address"},{"indexed":false,"name":"value","type":"uint256"}],"name":"Approval","type":"event"}]"""
    val template = s"""
        |[[Signature: EthereumEventFilter(
        |contract address: "0x531e0957391dabf46f8a9609d799ffd067bdbbc0";
        |interface: $abi;
        |event type name: "OpenlawSignatureEvent";
        |conditional filter: this.value = 2939)]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(template))
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters()
    )

    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    vm(LoadTemplate(template))

    val values = Map(
      VariableName("owner") -> EthAddressType
        .internalFormat(
          EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0").right.value
        )
        .right
        .value,
      VariableName("value") -> NumberType
        .internalFormat(BigDecimal(1000))
        .right
        .value
    )
    val identifier = vm.executionResult
      .map(_.allActions.right.value)
      .getOrElse(Seq())
      .head
      .identifier
    val event = EthereumEventFilterEvent(
      identifier.right.value,
      EthereumHash.empty,
      EthereumAddress("0x531E0957391dAbF46f8a9609d799fFD067bDbbC0").right.value,
      "OpenlawSignatureEvent",
      values,
      Instant.now
    )

    val ethereumEventFilterOracle =
      EthereumEventFilterOracle(parser, TestCryptoService)
    (for {
      result <- ethereumEventFilterOracle.incoming(vm, event)
    } yield result) match {
      case Right(vm) =>
        vm.allExecutions.keys shouldNot contain(VariableName("Signature"))
      case Failure(ex, message) =>
        fail(message, ex)
    }
  }

  it should "evaluate ethereum event filter properly" in {
    val abi =
      """[{"anonymous":false,"inputs":[{"indexed":false,"name":"value","type":"string"}],"name":"MyEvent","type":"event"}]"""

    val templateContent =
      s"""[[Id:Identity]]
                 |[[Contract Creation Event: EthereumEventFilter(
                 |contract address: "0x1234567889";
                 |interface: $abi;
                 |event type name: "MyEvent";
                 |conditional filter: this.value = "test")]]""".stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(templateContent))
    val email = Email("email@email.com").getOrThrow()
    val identity = Identity(email)

    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        "identity" -> IdentityType.internalFormat(identity).getOrThrow(),
        "param1" -> TextType.internalFormat("test value 1").getOrThrow(),
        "param2" -> TextType.internalFormat("test value 2").getOrThrow()
      )
    )

    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      Nil
    )
    vm(LoadTemplate(templateContent))

    val Some((varDef, varType)) = vm
      .getAllExecutedVariables(EthereumEventFilterType)
      .map({ case (execRes, varDef) => (varDef, varDef.varType(execRes)) })
      .headOption

    varDef.name shouldBe VariableName("Contract Creation Event")
    varType shouldBe EthereumEventFilterType
  }

  it should "execute an external call and display the result" in {
    val templateContent =
      """<%
        |[[param1:Text]]
        |[[param2:Text]]
        |[[externalCall:ExternalCall(
        |serviceName: "SomeIntegratedService";
        |parameters:
        | param1 -> param1,
        | param2 -> param2;
        |startDate: '2018-12-12 00:00:00';
        |endDate: '2048-12-12 00:00:00';
        |repeatEvery: '1 hour 30 minute')]]
        |%>
        |
        |[[identity:Identity]]
        |
        |[[externalCall.result.computationResult]]
      """.stripMargin

    val templateId = TemplateId(TestCryptoService.sha256(templateContent))
    val email = Email("email@email.com").getOrThrow()
    val identity = Identity(email)
    val definition = ContractDefinition(
      creatorId = UserId("hello@world.com"),
      mainTemplate = templateId,
      templates = Map.empty,
      parameters = TemplateParameters(
        "identity" -> IdentityType.internalFormat(identity).getOrThrow(),
        "param1" -> TextType.internalFormat("test value 1").getOrThrow(),
        "param2" -> TextType.internalFormat("test value 2").getOrThrow()
      )
    )

    val contractId = definition.id(TestCryptoService)
    val Success(abi) = variableTypes.IntegratedServiceDefinition(
      """[[Input:Structure(
        |param1: Text;
        |param2: Text
        |)]]
        |
        |[[Output:Structure(
        |computationResult:Text
        |)]]
        |""".stripMargin
    )

    val serviceName = ServiceName("SomeIntegratedService")
    val executionOracles = List(
      ExternalCallOracle(
        TestCryptoService,
        Map(serviceName -> serverAccount.address)
      )
    )
    val vm = vmProvider.create(
      definition,
      None,
      OpenlawSignatureOracle(TestCryptoService, serverAccount.address),
      executionOracles,
      Map(serviceName -> abi)
    )
    vm(LoadTemplate(templateContent))
    vm.executionResultState shouldBe ExecutionFinished
    vm.executionState shouldBe ContractCreated

    val signature = EthereumSignature(sign(identity, contractId).signature)
    val signatureEvent =
      oracles.OpenlawSignatureEvent(contractId, email, "", signature)
    vm(signatureEvent)

    val identifier = ActionIdentifier(
      "SomeIntegratedService#param1->test value 1#param2->test value 2"
    )
    val requestIdentifier = RequestIdentifier("test exec hash")
    val caller = Caller(contractId.id)
    val pendingExternalCallEvent = oracles.PendingExternalCallEvent(
      caller,
      identifier,
      requestIdentifier,
      Instant.now
    )
    vm(pendingExternalCallEvent)

    val output = abi.definedOutput
      .internalFormat(
        OpenlawMap(
          Map(VariableName("computationResult") -> OpenlawString("Hello World"))
        )
      )
      .getOrThrow()
    val eventSignature = serverAccount.sign(
      contractId.data
        .merge(EthereumData(identifier.identifier))
        .merge(EthereumData(output))
    )
    val successfulExternalCallEvent = oracles.SuccessfulExternalCallEvent(
      caller,
      identifier,
      requestIdentifier,
      Instant.now,
      output,
      serviceName,
      eventSignature
    )
    vm(successfulExternalCallEvent)

    vm.getAllExecutedVariables(ExternalCallType).size shouldBe 1

    val execution = variableTypes.SuccessfulExternalCallExecution(
      Instant.now,
      Instant.now,
      output,
      requestIdentifier
    )
    vm.newExecution(identifier, execution)

    vm.evaluate[OpenlawString]("externalCall.result.computationResult") match {
      case Success(result) =>
        result.underlying shouldBe "Hello World"
      case Failure(err, msg) =>
        fail(msg, err)
    }
  }

  def sign(identity: Identity, contractId: ContractId): EthereumSignature =
    signByEmail(identity.email, contractId.data, serverAccount)

  def signForStopping(
      identity: Identity,
      contractId: ContractId
  ): EthereumSignature =
    signByEmail(
      identity.email,
      contractId.stopContract(TestCryptoService),
      serverAccount
    )

  def signForResuming(
      identity: Identity,
      contractId: ContractId
  ): EthereumSignature = {
    signByEmail(
      identity.email,
      contractId.resumeContract(TestCryptoService),
      serverAccount
    )
  }

  private def signByEmail(
      email: Email,
      data: EthereumData,
      account: TestAccount
  ): EthereumSignature =
    EthereumSignature(
      account
        .sign(
          EthereumData(TestCryptoService.sha256(email.email))
            .merge(EthereumData(TestCryptoService.sha256(data.data)))
        )
        .signature
    )
}
