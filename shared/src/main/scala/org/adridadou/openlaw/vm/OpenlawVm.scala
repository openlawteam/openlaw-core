package org.adridadou.openlaw.vm

import java.time.{Clock, LocalDateTime, ZoneOffset}

import cats.Eq
import cats.implicits._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.values.{ContractDefinition, ContractId, TemplateId, TemplateParameters}
import org.adridadou.openlaw.oracles._
import org.adridadou.openlaw.result.{Failure, Result, Success}
import slogging.LazyLogging

import scala.reflect.ClassTag
import io.circe.generic.semiauto._
import LocalDateTimeHelper._
import org.adridadou.openlaw.{values, _}

case class Signature(userId:UserId, signature:OpenlawSignatureEvent)

object Executions {
  implicit val executionsEnc:Encoder[Executions] = deriveEncoder[Executions]
  implicit val executionsDec:Decoder[Executions] = deriveDecoder[Executions]
}

case class Executions(executionMap:Map[LocalDateTime,OpenlawExecution] = Map(), executionInit:Option[OpenlawExecutionInit] = None) {
  def update(key:LocalDateTime, value:OpenlawExecution):Executions =
    this.copy(executionMap = executionMap + (key -> value))

  def update(executionInit: OpenlawExecutionInit):Executions =
    this.copy(executionInit = Some(executionInit))
}

case class OpenlawVmState( contents:Map[TemplateId, String] = Map(),
                           templates:Map[TemplateId, CompiledTemplate] = Map(),
                           optExecutionResult:Option[OpenlawExecutionState],
                           definition:ContractDefinition,
                           profileAddress:Option[EthereumAddress],
                           state:TemplateParameters,
                           executions:Map[ActionIdentifier, Executions],
                           signatures:Map[Email, SignatureEvent],
                           signatureProofs:Map[Email, SignatureProof] = Map(),
                           events:List[OpenlawVmEvent] = List(),
                           executionEngine: OpenlawExecutionEngine,
                           executionState:ContractExecutionState,
                           crypto:CryptoService,
                           externalCallStructures: Map[ServiceName, IntegratedServiceDefinition] = Map(),
                           clock:Clock) extends LazyLogging {

  def updateTemplate(id: TemplateId, compiledTemplate: CompiledTemplate, event:LoadTemplate): OpenlawVmState =
    update(templates + (id -> compiledTemplate), state, executionResult, event)
      .copy(contents = contents + (id -> event.content))

  def updateExecutionState(newState: ContractExecutionState, event: OpenlawVmEvent): OpenlawVmState = {
    this.copy(executionState = newState, events = event :: events)
  }

  def updateParameter(name:VariableName, value:String, event:OpenlawVmEvent) :OpenlawVmState = {
    val newParams = state + (name -> value)
    update(templates, newParams, createNewExecutionResult(newParams, templates, signatureProofs, executions, externalCallStructures), event).copy(
      state = newParams
    )
  }

  def executionResult:Option[OpenlawExecutionState] = optExecutionResult match {
    case Some(result) => Some(result)
    case None => createNewExecutionResult(state, templates, signatureProofs, executions, externalCallStructures)
  }

  private def update(templates:Map[TemplateId, CompiledTemplate], parameters:TemplateParameters, optExecution:Option[OpenlawExecutionState], event:OpenlawVmEvent) : OpenlawVmState = {
    val execution = optExecution match {
      case Some(executionResult) =>
        Some(executionResult)
      case None =>
        createNewExecutionResult(parameters, templates, signatureProofs, executions, externalCallStructures)
    }

    execution.map(executeContract(templates, _) match {
      case Right(newResult) =>
        this.copy(optExecutionResult = Some(newResult))

      case Failure(ex, message) =>
        ex.printStackTrace()
        logger.warn(message, ex)
        this
    }).getOrElse(this).copy(
      templates = templates,
      events = event :: events
    )
  }

  private def executeContract(currentTemplates:Map[TemplateId, CompiledTemplate], execution: OpenlawExecutionState):Result[OpenlawExecutionState] = execution.state match {
    case ExecutionFinished =>
      Success(execution)

    case _ =>
      val templates = definition.templates.flatMap({case (templateDefinition, id) => currentTemplates.get(id).map(templateDefinition -> _)})
      executionEngine.resumeExecution(execution, templates)
  }

  def createNewExecutionResult(signatureProofs:Map[Email, SignatureProof], externalCallStructures: Map[ServiceName, IntegratedServiceDefinition]):Option[OpenlawExecutionState] =
    createNewExecutionResult(state, templates, signatureProofs, executions, externalCallStructures)

  def createNewExecutionResult(params:TemplateParameters, templates:Map[TemplateId, CompiledTemplate],signatureProofs:Map[Email, SignatureProof], executions:Map[ActionIdentifier, Executions], externalCallStructures: Map[ServiceName, IntegratedServiceDefinition]):Option[OpenlawExecutionState] = {
    val templateDefinitions = definition.templates.flatMap({case (templateDefinition, id) => templates.get(id).map(templateDefinition -> _)})
    templates.get(definition.mainTemplate).map(executionEngine.execute(_, params, templateDefinitions, signatureProofs, executions, externalCallStructures, Some(definition.id(crypto)), profileAddress)) match {
      case None => None
      case Some(Right(result)) =>
        Some(result)
      case Some(Left(ex)) =>
        ex.e.printStackTrace()
        logger.warn(ex.message, ex.e)
        None
    }
  }
}

case class OpenlawVm(contractDefinition: ContractDefinition, profileAddress:Option[EthereumAddress], crypto: CryptoService, parser:OpenlawTemplateLanguageParserService, identityOracle:OpenlawSignatureOracle, oracles:Seq[OpenlawOracle[_]], externalCallStructures: Map[ServiceName, IntegratedServiceDefinition] = Map()) extends LazyLogging {
  private val templateOracle = TemplateLoadOracle(crypto)
  val contractId:ContractId = contractDefinition.id(crypto)
  private val expressionParser = new ExpressionParserService

  private var state:OpenlawVmState = OpenlawVmState(
    state = contractDefinition.parameters,
    definition = contractDefinition,
    profileAddress = profileAddress,
    executionEngine = new OpenlawExecutionEngine(),
    executions = Map(),
    signatures = Map(),
    optExecutionResult = None,
    executionState = ContractCreated,
    clock = parser.internalClock,
    crypto = crypto,
    externalCallStructures = externalCallStructures,
  )

  def isSignatureValid(data:EthereumData, event:SignatureEvent): Result[Boolean] = {
    identityOracle.isSignatureValid(data, event)
  }

  def allIdentities: Result[Seq[Identity#T]] =
    state
      .executionResult
      .map { executionResult =>
        executionResult.allIdentities()
      }.getOrElse(Success(Seq()))

  def allNextActions: Result[Seq[ActionInfo]] =
    allActions.flatMap { actions =>
      actions
        .map { info =>
          info.identifier.flatMap { id => info.action.nextActionSchedule(info.executionResult, executions(id)).map(_.map(nextDate => (info, nextDate))) }
        }
        .toList
        .sequence
        .map(_.flatten)
    }
    .map(list => list.sortBy { case (_, nextDate) => nextDate.toEpochSecond(ZoneOffset.UTC) }.map { case (info, _) => info })

  def executionState:ContractExecutionState = state.executionState

  def nextActionSchedule: Result[Option[LocalDateTime]] =
    nextAction.flatMap { option =>
      option.flatMap { info =>
        info.identifier.flatMap { id =>
          info.action.nextActionSchedule(info.executionResult, executions(id))
        }.sequence
      }.sequence
    }

  def newSignature(email:Email, fullName:String, signature:SignatureEvent):OpenlawVm = {
    val signatureProofs = state.signatureProofs + (email -> signature.proof)

    val newExecutionResult = state.createNewExecutionResult(signatureProofs, state.externalCallStructures)
    state = state.copy(
      signatures = state.signatures + (email -> signature),
      signatureProofs = signatureProofs,
      optExecutionResult = newExecutionResult
    )
    this
  }

  def setInitExecution(identifier:ActionIdentifier, executionInit: OpenlawExecutionInit):OpenlawVm = {

    val executions = state.executions.getOrElse(identifier, Executions())
    val newExecutions = state.executions + (identifier  -> executions.update(executionInit))
    state = state.copy(executions = newExecutions)
    this
  }

  def executionDef:Map[ActionIdentifier, Executions] = state.executions

  def initExecution[T <: OpenlawExecutionInit](identifier:ActionIdentifier)(implicit classTag:ClassTag[T]): Result[Option[T]] =
    state
      .executions
      .get(identifier)
      .flatMap(_.executionInit)
      .map(VariableType.convert[T])
      .sequence

  def newExecution(identifier:ActionIdentifier, execution: OpenlawExecution):OpenlawVm = {
    val executions = state.executions.getOrElse(identifier, Executions())
    val newExecutions = state.executions + (identifier  -> executions.update(execution.scheduledDate, execution))
    execution.executionStatus match {
      case FailedExecution =>
        state = state.copy(executions = newExecutions, executionState = ContractStopped)
      case _ =>
        val newExecutionResult = state.createNewExecutionResult(state.state, state.templates, state.signatureProofs, newExecutions, state.externalCallStructures)
        state = state.copy(executions = newExecutions, optExecutionResult = newExecutionResult)
    }

    this
  }

  def allExecutions: Map[ActionIdentifier, Seq[OpenlawExecution]] = state.executions.map({case (identifier, executions) => identifier -> executions.executionMap.values.toSeq.sortBy(_.scheduledDate.toEpochSecond(ZoneOffset.UTC))})

  def executions[T <: OpenlawExecution](identifier: ActionIdentifier)(implicit classTag: ClassTag[T]): Seq[T] = allExecutions.getOrElse(identifier, Seq()).map(_.asInstanceOf[T])

  def allSignatures:Map[Email, SignatureEvent] = state.signatures
  def signature(email:Email):Option[SignatureEvent] = allSignatures.get(email)

  def events: Seq[OpenlawVmEvent] = state.events

  def nextAction: Result[Option[ActionInfo]] = allNextActions.map(_.headOption)

  def agreements:Seq[StructuredAgreement] =
    executionResult.map(_.agreements).getOrElse(Seq())

  def executionResultState:TemplateExecutionState =
    executionResult.map(_.state).getOrElse(ExecutionReady)

  def allActions: Result[Seq[ActionInfo]] = executionState match {
    case ContractCreated =>
      executionResult
        .map(_.allIdentityEmails)
        .sequence
        .map {
          _.getOrElse(Seq()).flatMap(email => generateSignatureAction(email))
        }
    case ContractRunning =>
      executionResult.map(_.allActions).getOrElse(Success(Seq()))
    case ContractResumed =>
      executionResult.map(_.allActions).getOrElse(Success(Seq()))
    case _ =>
      Success(Seq())
  }

  private def generateSignatureAction(email:Email):Option[ActionInfo] = {
    val services = (executedValues[ExternalSignature](ExternalSignatureType) match {
      case Seq() => Seq()
      case values => values.filter(_.identity.exists(_.email === email)).map(_.serviceName)
    }) ++ (executedValues[Identity](IdentityType) match {
      case Seq() => Seq()
      case values if values.exists(_.email === email) => Seq(ServiceName.openlawServiceName)
    })

    this.executionResult.map(executionResult => {
      executionResult.getAllExecutedVariables
      ActionInfo(SignatureAction(email, services.toList), executionResult)
    })
  }

  def template(definition: TemplateSourceIdentifier):CompiledTemplate = state.templates(contractDefinition.templates(definition))
  def template(id:TemplateId):CompiledTemplate = state.templates(id)
  def parameters:TemplateParameters = state.state

  def mainTemplate:CompiledTemplate = state.templates(contractDefinition.mainTemplate)

  def executionResult:Option[OpenlawExecutionState] = state.executionResult

  def content(templateId: TemplateId): String = state.contents(templateId)

  def getAllExecutedVariables(varType: VariableType): Seq[(TemplateExecutionResult, VariableDefinition)] =
    state
      .executionResult
      .map(_.getAllExecutedVariables)
      .getOrElse(Seq())
      .flatMap { case (result, variable) => variable.aliasOrVariable(result).map(expr => (result, expr)).toOption }
      .flatMap {
          case (r, variable: VariableDefinition) =>
            if (variable.varType(r) === varType) Some((r, variable)) else None
          case (_, _) => None
      }

  def getAllVariables(varTypes: VariableType*):Seq[(TemplateExecutionResult, VariableDefinition)] =
    state.executionResult.map(_.getVariables(varTypes:_*)).getOrElse(Seq())

  def getAllVariableValues[U <: OpenlawValue](varType: VariableType)(implicit classTag:ClassTag[U]): Result[Seq[(U#T, TemplateExecutionResult)]] =
    getAllVariables(varType)
      .toList
      .map { case (executionResult, variable) => variable.evaluate(executionResult).flatMap(_.map(VariableType.convert[U](_).map(u => u -> executionResult)).sequence) }
      .sequence
      .map(_.flatten)

  def parseExpression(expr:String): Result[Expression] = expressionParser.parseExpression(expr)

  def executedValues[T](variableType:VariableType)(implicit classTag:ClassTag[T]): Seq[T] = {
   getAllExecutedVariables(variableType)
      .filter({
        case (executionResult, variable) => variable.varType(executionResult) === variableType
      }).map({
        case (executionResult, variable) => variable.evaluate(executionResult)
      }).flatMap({
        case Success(Some(v:T)) => Some(v)
        case Success(Some(_)) => None
        case Success(None) => None
        case Failure(err, msg) =>
          logger.error(msg, err)
          None
      })
  }

  def evaluate[T](variable:VariableName)(implicit classTag:ClassTag[T]): Result[T] =
    evaluate(variable.name)

  def evaluate[T](expr:String)(implicit classTag:ClassTag[T]): Result[T] = {
    executionResult match {
      case Some(result) =>
        parseExpression(expr).flatMap(evaluate(result, _))
      case None =>
        Failure("the VM has not been executed yet!")
    }
  }

  def evaluate[T](expr:Expression)(implicit classTag:ClassTag[T]): Result[T] = {
    executionResult match {
      case Some(result) =>
        evaluate(result, expr)
      case None =>
        Failure("the VM has not been executed yet!")
    }
  }

  def evaluate[T](executionResult: TemplateExecutionResult, expr:String)(implicit classTag:ClassTag[T]): Result[T] = parseExpression(expr)
    .flatMap(evaluate[T](executionResult,_))

  def evaluate[T](executionResult: TemplateExecutionResult, expr:Expression)(implicit classTag:ClassTag[T]): Result[T] =
    expr.evaluate(executionResult).flatMap {
      case Some(value:T) => Success(value)
      case Some(value) => Failure(s"conversion error. Was expecting ${classTag.runtimeClass.getName} but got ${value.getClass.getName}")
      case None => Failure(s"could not resolve ${expr.toString}")
    }

  def applyEvent(event:OpenlawVmEvent): Result[OpenlawVm] = state.executionState match {
    case ContractCreated =>
      event match {
        case signature:SignatureEvent =>
          processSignature(signature)
        case e:LoadTemplate =>
          templateOracle.incoming(this, e)
        case e:OpenlawVmInitEvent =>
          executeEvent(e)
        case _ =>
          Failure("the virtual machine is in creation state. The only events allowed are signature events")
      }
    case _ => executeEvent(event)
  }

  def apply(event:OpenlawVmEvent):OpenlawVm = applyEvent(event) match {
    case Right(result) =>
      result
    case Left(ex) =>
      logger.warn(ex.message, ex.e)
      this
  }

  private def processSignature(event:SignatureEvent): Result[OpenlawVm] = {
    identityOracle.isSignatureValid(contractDefinition.id(crypto).data, event).flatMap(isValid =>
      if (isValid) {
        updateContractStateIfNecessary(this.newSignature(event.email, event.fullName, event), event)
      } else {
        Failure("Invalid event, ignoring!")
      })
  }

  private def updateContractStateIfNecessary(vm:OpenlawVm, event: OpenlawVmEvent): Result[OpenlawVm] = {
    vm.allNextActions.flatMap { nextActions =>
      vm.executionState match {
        case ContractCreated if nextActions.isEmpty =>
          vm(UpdateExecutionStateCommand(ContractRunning, event))
        case _ =>
          Success(vm)
      }
    }
  }

  private def executeEvent(event:OpenlawVmEvent): Result[OpenlawVm] = oracles.find(_.shouldExecute(event)) match {
    case Some(oracle) =>
      oracle.executeIfPossible(this, event)
    case None =>
      Failure(s"no oracle found! for event type ${event.getClass.getSimpleName}")
  }

  def apply(cmd:OpenlawVmCommand): Result[OpenlawVm] = cmd match {
    case LoadTemplateCommand(id, event) =>
      loadTemplate(id, event)
    case UpdateExecutionStateCommand(name, event) =>
      Right(updateExecutionState(name, event))
  }

  private def loadTemplate(id:TemplateId, event:LoadTemplate): Result[OpenlawVm] = {
    parser.compileTemplate(event.content).map(template => {
      state = state.updateTemplate(id, template , event)
      this
    })
  }

  private def updateExecutionState(name:ContractExecutionState, event:OpenlawVmEvent):OpenlawVm = {
    state = state.updateExecutionState(name, event)
    this
  }
}

trait OpenlawVmCommand

final case class LoadTemplateCommand(id:TemplateId, event:LoadTemplate) extends OpenlawVmCommand
final case class UpdateExecutionStateCommand(name:ContractExecutionState, event:OpenlawVmEvent) extends OpenlawVmCommand

case class ExecutionScope(template:CompiledTemplate, vm:OpenlawVm, clock:Clock)

abstract sealed class ContractExecutionState(val state:String)

case object ContractCreated extends ContractExecutionState("created")
case object ContractRunning extends ContractExecutionState("running")
case object ContractStopped extends ContractExecutionState("stopped")
case object ContractResumed extends ContractExecutionState("resumed")

object ContractExecutionState {

  def apply(name:String):ContractExecutionState = name match {
    case ContractCreated.state => ContractCreated
    case ContractRunning.state => ContractRunning
    case ContractStopped.state => ContractStopped
    case ContractResumed.state => ContractResumed
  }

  implicit val contractExecutionStateEnc:Encoder[ContractExecutionState] = (a: ContractExecutionState) => Json.fromString(a.state)
  implicit val contractExecutionStateDec:Decoder[ContractExecutionState] = (c: HCursor) => c.as[String].map(state => ContractExecutionState(state))

  implicit val eqForExecutionState: Eq[ContractExecutionState] = Eq.fromUniversalEquals
}
