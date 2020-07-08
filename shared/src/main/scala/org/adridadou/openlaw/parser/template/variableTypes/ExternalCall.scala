package org.adridadou.openlaw.parser.template.variableTypes

import java.time.temporal.ChronoUnit
import java.time.Instant

import cats.implicits._
import cats.kernel.Eq
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe._
import org.adridadou.openlaw.oracles.CryptoService
import org.adridadou.openlaw.{
  OpenlawInstant,
  OpenlawNativeValue,
  OpenlawString,
  OpenlawValue,
  result
}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes.LocalDateTimeHelper._
import org.adridadou.openlaw.parser.template.variableTypes.VariableType._
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.result.Implicits._
import org.adridadou.openlaw.values.ContractId
import org.adridadou.openlaw.vm.OpenlawExecutionEngine
import scala.collection.JavaConverters._

object IntegratedServiceDefinition {
  val parser = new OpenlawTemplateLanguageParserService()
  val engine = new OpenlawExecutionEngine()
  private val signatureDefinitionStr =
    """
      |[[Input:Structure(contractContentBase64: Text; contractTitle: Text; accessToken: Text; tokenExpiry: Number; refreshToken: Text; signerEmailsJson: Text)]]
      |[[Output:Structure(signerEmailsJson: Text; signaturesJson: Text; recordLink: Text; pdfContentsBase64: Text)]]
    """.stripMargin

  private val storageDefinitionStr =
    "[[Input:Structure(accessToken: Text; operation: Text; filePath: Text; fileBase64Content: Text)]] " +
      "[[Output:Structure(filePath: Text; fileName: Text; fileBase64Content: Text)]]"

  val signatureDefinition: IntegratedServiceDefinition =
    IntegratedServiceDefinition(signatureDefinitionStr).getOrThrow()
  val storageDefinition: IntegratedServiceDefinition =
    IntegratedServiceDefinition(storageDefinitionStr).getOrThrow()

  def apply(definition: String): result.Result[IntegratedServiceDefinition] = {
    val result = for {
      i <- getStructure(definition, "Input")
      o <- getStructure(definition, "Output")
    } yield new IntegratedServiceDefinition(i, o)

    result
  }

  private def getStructure(
      input: String,
      variableTypeName: String
  ): result.Result[Structure] =
    parser
      .compileTemplate(input)
      .flatMap(template => engine.execute(template))
      .map(_.findVariableType(VariableTypeDefinition(variableTypeName)))
      .flatMap({
        case Some(t: DefinedStructureType) => Success(t.structure)
        case Some(_)                       => Failure("invalid type")
        case None                          => Failure(s"no type found named $variableTypeName")
      })

  implicit val integratedServiceDefinitionEnc
      : Encoder[IntegratedServiceDefinition] = deriveEncoder
  implicit val integratedServiceDefinitionDec
      : Decoder[IntegratedServiceDefinition] = deriveDecoder
  implicit val integratedServiceDefinitionEq: Eq[IntegratedServiceDefinition] =
    Eq.fromUniversalEquals
}

object ServiceName {
  implicit val serviceNameEnc: Encoder[ServiceName] = (sn: ServiceName) =>
    Json.fromString(sn.serviceName)
  implicit val serviceNameDec: Decoder[ServiceName] = (c: HCursor) =>
    c.as[String].map(ServiceName(_))
  implicit val serviceNameKeyEnc: KeyEncoder[ServiceName] =
    (key: ServiceName) => key.serviceName
  implicit val serviceNameKeyDec: KeyDecoder[ServiceName] = (key: String) =>
    Some(ServiceName(key))

  implicit val serviceNameEq: Eq[ServiceName] = Eq.fromUniversalEquals

  val openlawServiceName: ServiceName = ServiceName("Openlaw")
}

final case class ServiceName(serviceName: String)

final case class IntegratedServiceDefinition(
    input: Structure,
    output: Structure
) {
  def definedInput: DefinedStructureType = DefinedStructureType(input, "Input")
  def definedOutput: DefinedStructureType =
    DefinedStructureType(output, "Output")
}
sealed trait StorageOperation {
  val id: String
}
object StorageOperation {
  def apply(id: String): StorageOperation = id match {
    case StorageWriteOp.id => StorageWriteOp
    case StorageReadOp.id  => StorageReadOp
  }
}
case object StorageWriteOp extends StorageOperation {
  val id: String = "write"
}
case object StorageReadOp extends StorageOperation {
  val id: String = "read"
}
final case class StorageInput(
    accessToken: String,
    operation: StorageOperation,
    filePath: String,
    fileBase64Content: Option[String]
)
object StorageInput {
  implicit val signatureInputEnc: Encoder[StorageInput] =
    Encoder.instance[StorageInput] { input =>
      Json.obj(
        "accessToken" -> Json.fromString(input.accessToken),
        "operation" -> Json.fromString(input.operation.id),
        "filePath" -> Json.fromString(input.filePath),
        "fileBase64Content" -> Json.fromString(
          input.fileBase64Content.getOrElse[String]("")
        )
      )
    }
  implicit val signatureInputDec: Decoder[StorageInput] =
    Decoder.instance[StorageInput] { c: HCursor =>
      for {
        accessToken <- c.downField("accessToken").as[String]
        operation <- c
          .downField("operation")
          .as[String]
          .map(StorageOperation(_))
        filePath <- c.downField("filePath").as[String]
        fileBase64Content <- c.downField("fileBase64Content").as[Option[String]]
      } yield StorageInput(
        accessToken,
        operation,
        filePath,
        fileBase64Content
      )
    }
  implicit val storageInputEq: Eq[StorageInput] = Eq.fromUniversalEquals
}
final case class StorageOutput(
    filePath: String,
    fileName: String,
    fileBase64Content: Option[String]
)
object StorageOutput {
  implicit val storageOutputEnc: Encoder[StorageOutput] = deriveEncoder
  implicit val storageOutputDec: Decoder[StorageOutput] = deriveDecoder
  implicit val storageOutputEq: Eq[StorageOutput] = Eq.fromUniversalEquals
}

final case class Signatory(
    signaturePlaceholderText: String,
    email: String,
    name: String
)

object Signatory {
  implicit val enc: Encoder[Signatory] = deriveEncoder
  implicit val dev: Decoder[Signatory] = deriveDecoder
}

final case class SignatureInput(
    // Json representation of List[Signatory]
    signerEmailsJson: String,
    contractContentBase64: String,
    contractTitle: String,
    accessToken: String,
    // When the token will expire as a unix timestamp
    tokenExpiry: Long,
    // The token to refresh the access token
    refreshToken: String
)

object SignatureInput {
  implicit val signatureInputDec: Decoder[SignatureInput] = deriveDecoder

  implicit val signatureInputEnc: Encoder[SignatureInput] = deriveEncoder
}

/**
  * A External Call response indicating that the response could tkae an extended amount time to
  * arrive (due to waiting on some action by a user, for example), and the ultimate completed response
  * will be given asynchronously.
  *
  * @param expires when the request should be considered failed if no response comes before this time.
  */
final case class DeferredResponse(
    // If a response isn't received by this time, the caller should consider the request to be expired
    expires: Option[Instant] = None
)

object DeferredResponse {
  implicit val deferredResponseDec: Decoder[DeferredResponse] = deriveDecoder
  implicit val deferredResponseEnc: Encoder[DeferredResponse] = deriveEncoder
}

final case class SignatureOutput(
    // Json representation of List[Email]
    signerEmailsJson: String,
    // Json representation of List[EthereumSignature]
    signaturesJson: String,
    recordLink: String,
    pdfContentsBase64: String
)
object SignatureOutput {
  implicit val signatureOutputEnc: Encoder[SignatureOutput] = deriveEncoder
  implicit val signatureOutputDec: Decoder[SignatureOutput] = deriveDecoder

  implicit val signatureOutputEq: Eq[SignatureOutput] = Eq.fromUniversalEquals

  def prepareDataToSign(
      email: Email,
      contractId: ContractId,
      cryptoService: CryptoService
  ): EthereumData = {
    EthereumData(cryptoService.sha256(email.email))
      .merge(EthereumData(cryptoService.sha256(contractId.data.data)))
  }
}

final case class ExternalCall(
    serviceName: Expression,
    parameters: Map[VariableName, Expression],
    startDate: Option[Expression],
    endDate: Option[Expression],
    every: Option[Expression]
) extends ActionValue
    with OpenlawNativeValue {

  override def identifier(
      executionResult: TemplateExecutionResult
  ): Result[ActionIdentifier] =
    serviceName.evaluateT[OpenlawString](executionResult).flatMap {
      valueOption =>
        parameters.toList
          .sortBy { case (key, _) => key.name }
          .map {
            case (key, v) =>
              v.evaluate(executionResult)
                .map(option => option.map(key.name + "->" + _).getOrElse(""))
          }
          .sequence
          .map { values =>
            val value = valueOption.getOrElse("") + "#" + values.mkString("#")
            ActionIdentifier(value)
          }
    }

  def getServiceName(executionResult: TemplateExecutionResult): Result[String] =
    getString(serviceName, executionResult)

  def getParameters(
      executionResult: TemplateExecutionResult
  ): Result[Map[VariableName, OpenlawValue]] =
    parameters
      .map {
        case (name, expr) => expr.evaluate(executionResult).map(name -> _)
      }
      .toList
      .sequence
      .map(_.collect { case (name, Some(value)) => name -> value }.toMap)

  def getStartDate(
      executionResult: TemplateExecutionResult
  ): Result[Option[Instant]] =
    startDate.map(getDate(_, executionResult).map(_.underlying)).sequence

  def getEndDate(
      executionResult: TemplateExecutionResult
  ): Result[Option[Instant]] =
    endDate.map(getDate(_, executionResult).map(_.underlying)).sequence

  def getEvery(
      executionResult: TemplateExecutionResult
  ): Result[Option[Period]] =
    every.map(getPeriod(_, executionResult)).sequence

  private def callToRerun(
      executions: List[ExternalCallExecution],
      executionResult: TemplateExecutionResult
  ): Option[Instant] =
    executions
      .find { execution =>
        execution.executionStatus match {
          case FailedExecution =>
            execution.executionDate
              .isBefore(
                executionResult.info.now
                  .minus(5, ChronoUnit.MINUTES)
              )
          case _ =>
            false
        }
      }
      .map(_.scheduledDate)

  private def getNextScheduledRun(
      executions: List[ExternalCallExecution],
      executionResult: TemplateExecutionResult
  ): Result[Option[Instant]] =
    executions.map(_.scheduledDate) match {
      case Nil =>
        getStartDate(executionResult)
          .map(_.orElse(Some(executionResult.info.now)))
      case list =>
        val lastDate = list.maxBy(_.getEpochSecond)
        for {
          schedulePeriodOption <- getEvery(executionResult)
          endDate <- getEndDate(executionResult)
          nextDateOpt <- DateTimeType.plus(
            StringConstant("last date"),
            Some(OpenlawInstant(lastDate)),
            DateTimeType,
            schedulePeriodOption,
            executionResult
          )
        } yield nextDateOpt.flatMap({
          case nextDate: OpenlawInstant =>
            filterDateAfterEndDate(endDate, nextDate.underlying)
          case _ => None
        })
    }

  private def filterDateAfterEndDate(
      endDate: Option[Instant],
      nextDate: Instant
  ): Option[Instant] =
    if (endDate.forall(date =>
          nextDate
            .isBefore(date) || nextDate === date
        )) {
      Some(nextDate)
    } else {
      None
    }

  override def nextActionSchedule(
      executionResult: TemplateExecutionResult,
      pastExecutions: List[OpenlawExecution]
  ): Result[Option[Instant]] =
    for {
      executions <- pastExecutions
        .map(VariableType.convert[ExternalCallExecution])
        .sequence
      rerunSchedule = callToRerun(executions, executionResult)
      nextCall <- getNextScheduledRun(executions, executionResult)
    } yield rerunSchedule.orElse(nextCall)
}

object ExternalCall {
  implicit val externalCallEnc: Encoder[ExternalCall] = deriveEncoder
  implicit val externalCallDec: Decoder[ExternalCall] = deriveDecoder
}
