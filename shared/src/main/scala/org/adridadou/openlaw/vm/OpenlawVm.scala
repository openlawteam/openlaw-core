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
import org.adridadou.openlaw._

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
                           state:TemplateParameters,
                           executions:Map[VariableName, Executions],
                           signatures:Map[Email, OpenlawSignatureEvent],
                           signatureProofs:Map[Email, OpenlawSignatureProof] = Map(),
                           events:List[OpenlawVmEvent] = List(),
                           executionEngine: OpenlawExecutionEngine,
                           executionState:ContractExecutionState,
                           crypto:CryptoService,
                           clock:Clock) extends LazyLogging {

  def updateTemplate(id: TemplateId, compiledTemplate: CompiledTemplate, event:LoadTemplate): OpenlawVmState =
    update(templates + (id -> compiledTemplate), state, executionResult, event)
      .copy(contents = contents + (id -> event.content))

  def updateExecutionState(newState: ContractExecutionState, event: OpenlawVmEvent): OpenlawVmState = {
    this.copy(executionState = newState, events = event :: events)
  }

  def updateParameter(name:VariableName, value:String, event:OpenlawVmEvent) :OpenlawVmState = {
    val newParams = state + (name -> value)
    update(templates, newParams, createNewExecutionResult(newParams, templates, signatureProofs, executions), event).copy(
      state = newParams
    )
  }

  def executionResult:Option[OpenlawExecutionState] = optExecutionResult match {
    case Some(result) => Some(result)
    case None => createNewExecutionResult(state, templates, signatureProofs, executions)
  }

  private def update(templates:Map[TemplateId, CompiledTemplate], parameters:TemplateParameters, optExecution:Option[OpenlawExecutionState], event:OpenlawVmEvent) : OpenlawVmState = {
    val execution = optExecution match {
      case Some(executionResult) =>
        Some(executionResult)
      case None =>
        createNewExecutionResult(parameters, templates, signatureProofs, executions)
    }

    execution.map(executeContract(templates, _) match {
      case Right(newResult) =>
        this.copy(optExecutionResult = Some(newResult))

      case Left(ex) =>
        logger.warn(ex.message, ex.e)
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

  def createNewExecutionResult(signatureProofs:Map[Email, OpenlawSignatureProof]):Option[OpenlawExecutionState] =
    createNewExecutionResult(state, templates, signatureProofs, executions)

  def createNewExecutionResult(params:TemplateParameters, templates:Map[TemplateId, CompiledTemplate],signatureProofs:Map[Email, OpenlawSignatureProof], executions:Map[VariableName, Executions]):Option[OpenlawExecutionState] = {
    val templateDefinitions = definition.templates.flatMap({case (templateDefinition, id) => templates.get(id).map(templateDefinition -> _)})
    templates.get(definition.mainTemplate).map(executionEngine.execute(_, params, templateDefinitions, signatureProofs, executions, Some(definition.id(crypto)))) match {
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

case class OpenlawVm(contractDefinition: ContractDefinition, cryptoService: CryptoService, parser:OpenlawTemplateLanguageParserService, identityOracle:OpenlawSignatureOracle, oracles:Seq[OpenlawOracle[_]]) extends LazyLogging {
  private val templateOracle = TemplateLoadOracle(cryptoService)
  val contractId:ContractId = contractDefinition.id(cryptoService)
  private val expressionParser = new ExpressionParserService

  private var state:OpenlawVmState = OpenlawVmState(
    state = contractDefinition.parameters,
    definition = contractDefinition,
    executionEngine = new OpenlawExecutionEngine(),
    executions = Map(),
    signatures = Map(),
    optExecutionResult = None,
    executionState = ContractCreated,
    clock = parser.internalClock,
    crypto = cryptoService
  )

  def isSignatureValid(data:EthereumData, event:OpenlawSignatureEvent): Result[Boolean] = {
    identityOracle.isSignatureValid(data, event)
  }

  def allIdentities: Result[Seq[Identity#T]] =
    state
      .executionResult
      .map { executionResult =>
        executionResult
          .getAllExecutedVariables
          .flatMap { case (result, name) => result.getVariable(name).map(variable => (result, variable)) }
          .map { case (result, variable) =>
            variable.varType(result) match {
              case IdentityType =>
                variable.evaluate(result).flatMap(_.map(VariableType.convert[Identity]).sequence).map(_.toList)
              case collectionType: CollectionType if collectionType.typeParameter === IdentityType =>
                variable
                  .evaluate(result)
                  .flatMap(_.map(VariableType.convert[CollectionValue]).sequence)
                  .flatMap { option =>
                    option
                      .map(_.list)
                      .getOrElse(Seq())
                      .map(VariableType.convert[Identity])
                      .toList
                      .sequence
                  }
              case structureType:DefinedStructureType if structureType.structure.typeDefinition.values.exists(_ === IdentityType) =>
                variable
                  .evaluate(result)
                  .flatMap(_.map(VariableType.convert[OpenlawMap[VariableName, OpenlawValue]]).sequence)
                  .map(_.getOrElse(Map()))
                  .flatMap { values =>
                    structureType
                      .structure
                      .typeDefinition
                      .map {
                        case (name, varType) if varType === IdentityType =>
                          values.get(name).map(VariableType.convert[Identity]).sequence
                        case _ =>
                          Success[Option[Identity#T]](None)
                      }
                      .toList
                      .sequence
                      .map(_.flatten)
                  }

              case _ =>
                Success(List())
            }
          }
          .toList
          .sequence
          .map(_.flatten)
      }
      .sequence
      .map(_.getOrElse(List()))

  def allNextActions: Result[Seq[ActionInfo]] = allActions
    .flatMap { seq =>
      seq
        .map { info => info.action.nextActionSchedule(info.executionResult, executions(info.name)).map(_.map(nextDate => (info, nextDate))) }
        .toList
        .sequence
        .map(_.flatten)
    }
    .map(list => list.sortBy { case (_, nextDate) => nextDate.toEpochSecond(ZoneOffset.UTC) }.map { case (info, _) => info })

  def executionState:ContractExecutionState = state.executionState

  def nextActionSchedule: Result[Option[LocalDateTime]] =
    nextAction.flatMap(_.flatMap(info => info.action.nextActionSchedule(info.executionResult, executions(info.name)).sequence).sequence)

  def newSignature(identity:Identity, fullName:String, signature:OpenlawSignatureEvent):OpenlawVm = {
    val email = identity.email
    val signatureProofs = state.signatureProofs + (email -> signature.proof)
    val newExecutionResult = state.createNewExecutionResult(signatureProofs)
    state = state.copy(
      signatures = state.signatures + (email -> signature),
      signatureProofs = signatureProofs,
      optExecutionResult = newExecutionResult
    )
    this
  }

  def setInitExecution(name:VariableName, executionInit: OpenlawExecutionInit):OpenlawVm = {
    val executions = state.executions.getOrElse(name, Executions())
    val newExecutions = state.executions + (name  -> executions.update(executionInit))
    state = state.copy(executions = newExecutions)

    this
  }

  def initExecution[T <: OpenlawExecutionInit](name:VariableName)(implicit classTag:ClassTag[T]): Result[Option[T]] =
    state
      .executions
      .get(name)
      .flatMap(_.executionInit)
      .map(VariableType.convert[T])
      .sequence

  def newExecution(name:VariableName, execution: OpenlawExecution):OpenlawVm = {
    val executions = state.executions.getOrElse(name, Executions())
    val newExecutions = state.executions + (name  -> executions.update(execution.scheduledDate, execution))
    execution.executionStatus match {
      case FailedExecution =>
        state = state.copy(executions = newExecutions, executionState = ContractStopped)
      case _ =>
        val newExecutionResult = state.createNewExecutionResult(state.state, state.templates, state.signatureProofs, newExecutions)
        state = state.copy(executions = newExecutions, optExecutionResult = newExecutionResult)
    }

    this
  }

  def allExecutions: Map[VariableName, Seq[OpenlawExecution]] = state.executions.map({case (name, executions) => name -> executions.executionMap.values.toSeq.sortBy(_.scheduledDate.toEpochSecond(ZoneOffset.UTC))})

  def executions[T <: OpenlawExecution](name: VariableName)(implicit classTag: ClassTag[T]): Seq[T] = allExecutions.getOrElse(name, Seq()).map(_.asInstanceOf[T])

  def allSignatures:Map[Email, OpenlawSignatureEvent] = state.signatures
  def signature(email:Email):Option[OpenlawSignatureEvent] = allSignatures.get(email)

  def events: Seq[OpenlawVmEvent] = state.events

  def nextAction: Result[Option[ActionInfo]] = allNextActions.map(_.headOption)

  def agreements:Seq[StructuredAgreement] =
    executionResult.map(_.agreements).getOrElse(Seq())

  def executionResultState:TemplateExecutionState =
    executionResult.map(_.state).getOrElse(ExecutionReady)

  def allActions: Result[Seq[ActionInfo]] = executionState match {
    case ContractRunning =>
      executionResult.map(_.allActions()).getOrElse(Success(Seq()))
    case ContractResumed =>
      executionResult.map(_.allActions()).getOrElse(Success(Seq()))
    case ContractCreated =>
      executionResult
        .map(_.allIdentityEmails)
        .sequence
        .map {
          _.getOrElse(Seq()).flatMap(email => generateSignatureAction(email))
        }
    case _ =>
      Success(Seq())
  }

  private def generateSignatureAction(email:Email):Option[ActionInfo] =
    this.executionResult.map(ActionInfo(SignatureAction(email), VariableName(""), _))

  def template(definition: TemplateSourceIdentifier):CompiledTemplate = state.templates(contractDefinition.templates(definition))
  def template(id:TemplateId):CompiledTemplate = state.templates(id)
  def parameters:TemplateParameters = state.state

  def mainTemplate:CompiledTemplate = state.templates(contractDefinition.mainTemplate)

  def executionResult:Option[OpenlawExecutionState] = state.executionResult

  def content(templateId: TemplateId): String = state.contents(templateId)

  def getAllVariables(varType: VariableType):Seq[(TemplateExecutionResult, VariableDefinition)] =
    state.executionResult.map(_.getVariables(varType)).getOrElse(Seq())

  def getAllVariableValues[U <: OpenlawValue](varType: VariableType)(implicit classTag:ClassTag[U]): Result[Seq[U#T]] =
    getAllVariables(varType)
      .toList
      .map { case (executionResult, variable) => variable.evaluate(executionResult).flatMap(_.map(VariableType.convert[U]).sequence) }
      .sequence
      .map(_.flatten)

  def parseExpression(expr:String): Result[Expression] = expressionParser.parseExpression(expr)

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
        case signature:OpenlawSignatureEvent =>
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

  private def processSignature(event:OpenlawSignatureEvent): Result[OpenlawVm] = {
    getAllVariables(IdentityType)
      .map({case (executionResult,variable) => (variable.name, evaluate[Identity](executionResult, variable.name))})
      .filter({
        case (_, Right(identity)) => event.email === identity.email
        case _ => false
      }).toList match {
      case Nil =>
        Failure(s"invalid event! no matching identity for the signature. identity:${event.email}")
      case users =>
        val initialValue: Result[OpenlawVm] = Success(this)
        users.foldLeft(initialValue)({
          case (Right(currentVm), (_, Right(identity))) =>
            updateContractStateIfNecessary(currentVm.newSignature(identity, event.fullName, event), event)
          case _ =>
            Failure("error while processing identity")
        })
    }
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
      logger.warn(s"no oracle found! for event type ${event.getClass.getSimpleName}")
      Right(this)
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