package org.adridadou.openlaw.parser.template.variableTypes

import java.time.{Clock, LocalDateTime, ZoneId, ZoneOffset}

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template.formatters.{DefaultFormatter, Formatter}
import cats.Eq
import cats.implicits._
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._

import scala.reflect.ClassTag
import scala.util.Try
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import cats.data.EitherT
import org.adridadou.openlaw.oracles.{EthereumEventFilterExecution, PreparedERC712SmartContractCallExecution}
import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.variableTypes.VariableType.convert

trait NoShowInForm

trait ActionValue {
  def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions:Seq[OpenlawExecution]): Result[Option[LocalDateTime]]
}

trait ActionType extends NoShowInForm {
  def actionValue(value:OpenlawValue): Result[ActionValue]
}

object OpenlawExecution {
  implicit val openlawExecutionEnc:Encoder[OpenlawExecution] = (a: OpenlawExecution) => Json.obj(
    "type" -> Json.fromString(a.typeIdentifier),
    "value" -> a.serialize
  )
  implicit val openlawExecutionDec:Decoder[OpenlawExecution] = (c: HCursor) => c.downField("type").as[String]
    .flatMap(convertOpenlawExecution(_, c.downField("value")))

  protected def className[T]()(implicit cls:ClassTag[T]):String = cls.runtimeClass.getName

  private def convertOpenlawExecution(typeDefinition: String, cursor: ACursor):Decoder.Result[OpenlawExecution] = typeDefinition match {
    case _ if typeDefinition === className[EthereumEventFilterExecution] =>
      cursor.as[EthereumEventFilterExecution]
    case _ if typeDefinition === className[EthereumSmartContractExecution] =>
      cursor.as[EthereumSmartContractExecution]
  }
}

object OpenlawExecutionInit {
  implicit val openlawExecutionInitEnc:Encoder[OpenlawExecutionInit] = (a: OpenlawExecutionInit) => Json.obj(
    "type" -> Json.fromString(a.typeIdentifier),
    "value" -> Json.fromString(a.serialize)
  )
  implicit val openlawExecutionDec:Decoder[OpenlawExecutionInit] = (c: HCursor) => c.downField("type").as[String]
    .flatMap(convertOpenlawExecutionInit(_, c.downField("value")))

  protected def className[T]()(implicit cls:ClassTag[T]):String = cls.runtimeClass.getName

  private def convertOpenlawExecutionInit(typeDefinition: String, cursor: ACursor):Decoder.Result[OpenlawExecutionInit] = typeDefinition match {
    case _ if typeDefinition === className[PreparedERC712SmartContractCallExecution] =>
      cursor.as[PreparedERC712SmartContractCallExecution]
  }
}

trait OpenlawExecutionInit extends OpenlawNativeValue {
  protected def className[T]()(implicit cls:ClassTag[T]):String = cls.runtimeClass.getName

  def typeIdentifier: String
  def serialize: String
}

trait OpenlawExecution extends OpenlawNativeValue {
  def scheduledDate:LocalDateTime
  def executionDate:LocalDateTime
  def executionStatus:OpenlawExecutionStatus
  def key:Any
  def typeIdentifier: String
  def serialize: Json
  protected def className[T]()(implicit cls:ClassTag[T]):String = cls.runtimeClass.getName
}

object EthereumSmartContractExecution {
  implicit val smartContractExecutionEnc: Encoder[EthereumSmartContractExecution] = deriveEncoder[EthereumSmartContractExecution]
  implicit val smartContractExecutionDec: Decoder[EthereumSmartContractExecution] = deriveDecoder[EthereumSmartContractExecution]
}

case class EthereumSmartContractExecution(scheduledDate:LocalDateTime, executionDate:LocalDateTime, executionStatus: OpenlawExecutionStatus = PendingExecution, tx:EthereumHash) extends OpenlawExecution {
  def message: String = executionStatus match {
    case PendingExecution => "the transaction has been submitted, waiting for the transaction to be executed"
    case SuccessfulExecution => "the transaction has been added to the chain and successfully executed"
    case FailedExecution => "the transaction execution has failed"
  }

  def key:EthereumHash = tx

  override def typeIdentifier: String = className[EthereumSmartContractExecution]
  override def serialize: Json = this.asJson
}

sealed abstract class OpenlawExecutionStatus(val name:String)

case object PendingExecution extends OpenlawExecutionStatus("pending")
case object SuccessfulExecution extends OpenlawExecutionStatus("success")
case object FailedExecution extends OpenlawExecutionStatus("failed")

object OpenlawExecutionStatus {

  def apply(name:String):OpenlawExecutionStatus = name match {
    case "success" => SuccessfulExecution
    case "failed" => FailedExecution
    case _ => PendingExecution
  }

  implicit val executionStatusDecoder: Decoder[OpenlawExecutionStatus] = (c: HCursor) => {
    for {
      name <- c.as[String]
    } yield OpenlawExecutionStatus(name)
  }
  implicit val executionStatusEncoder: Encoder[OpenlawExecutionStatus] = (a: OpenlawExecutionStatus) => Json.fromString(a.name)

  implicit val eqForExecutionStatus: Eq[OpenlawExecutionStatus] = Eq.fromUniversalEquals
}

trait ParameterType {
  val typeParameter:VariableType
}

trait ParameterTypeProvider {
  def createParameterInstance(parameterType:VariableType):VariableType with ParameterType
}

abstract class VariableType(val name: String) {

  def serialize: Json = Json.obj("name" -> io.circe.Json.fromString(name))

  def validateOperation(expr: ValueExpression, executionResult: TemplateExecutionResult): Option[String] = None

  def accessVariables(name:VariableName, keys:Seq[String], executionResult: TemplateExecutionResult): Result[Seq[VariableName]] =
    Success(Seq(name))

  def operationWith(rightType: VariableType, operation: ValueOperation): VariableType =
    this

  def access(value: OpenlawValue, variableName:VariableName, keys: Seq[String], executionResult:TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    if(keys.isEmpty) {
      Success(Some(value))
    } else {
      Failure(s"The variable $variableName of type $name has no properties")
    }
  }

  def getTypeClass: Class[_ <: OpenlawValue]

  def checkTypeName(nameToCheck: String): Boolean =
    this.name.equalsIgnoreCase(nameToCheck)

  def validateKeys(variableName:VariableName, keys:Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] =
    keys.headOption.map(_ => Failure(s"The variable $variableName of type $name has no properties")).getOrElse(Success(()))

  def keysType(keys: Seq[String], expression: Expression, executionResult: TemplateExecutionResult):Result[VariableType] = if(keys.nonEmpty) {
    Failure(s"the type $name has no properties")
  } else {
    Success(thisType)
  }

  def combineConverted[U <: OpenlawValue, Y <: OpenlawValue]
    (optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue])
    (operation: PartialFunction[(U#T, U#T), Result[Y]])
    (implicit ct: ClassTag[U])
  : Result[Option[Y]] =
    combineConverted[U, U, Y](optLeft, optRight)(operation)

  def combineConverted[U <: OpenlawValue, V <: OpenlawValue, Y <: OpenlawValue]
    (optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue])
    (operation: PartialFunction[(U#T, V#T), Result[Y]])
    (implicit ct1: ClassTag[U], ct2: ClassTag[V])
  : Result[Option[Y]] = {
    (for {
      left <- EitherT(optLeft.map(convert[U]))
      right <- EitherT(optRight.map(convert[V]))
    } yield {
      if (operation.isDefinedAt(left -> right)) operation(left -> right)
      else Failure(s"no matching case in partial function for arguments $left and $right")
    })
      .value
      .map(_.flatten)
      .sequence
  }

  def combine[Y <: OpenlawValue](optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue])(operation: PartialFunction[(OpenlawValue, OpenlawValue), Result[Y]]): Result[Option[Y]] = {
    (for {
      left <- optLeft
      right <- optRight
    } yield {
      if (operation.isDefinedAt(left -> right)) operation(left -> right)
      else Failure(s"no matching case in partial function for arguments $left and $right")
    })
      .sequence
  }

  def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    Failure(new UnsupportedOperationException(s"$name type does not support addition"))
  def minus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    Failure(new UnsupportedOperationException(s"$name type does not support substraction"))
  def multiply(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    Failure(new UnsupportedOperationException(s"$name type does not support multiplication"))
  def divide(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
    Failure(new UnsupportedOperationException(s"$name type does not support division"))

  def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean =
    otherType === this

  def cast(value: String, executionResult:TemplateExecutionResult):Result[OpenlawValue]

  def missingValueFormat(name: VariableName): Seq[AgreementElement] = Seq(FreeText(Text(s"[[${name.name}]]")))

  def internalFormat(value: OpenlawValue): Result[String]

  def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = constructorParams match {
      case OneValueParameter(expr) =>
        expr.evaluate(executionResult)
      case Parameters(parameterMap) =>
        parameterMap.toMap.get("value") match {
          case Some(parameter) =>
            attempt(construct(parameter, executionResult)).flatten
          case None => Success(None)
        }
      case _ =>
        Failure(s"the constructor for $name only handles single values")
    }

  def defaultFormatter: Formatter =
    new DefaultFormatter

  def getFormatter(name:FormatterDefinition, executionResult:TemplateExecutionResult): Result[Formatter] = Success(defaultFormatter)

  def getSingleParameter(constructorParams: Parameter): Expression =
    constructorParams match {
      case OneValueParameter(expr) => expr
      case _ =>
        throw new RuntimeException("expecting a single value")
    }

  def handleTry[T](thisTry: Try[T]): T =
    thisTry match {
      case scala.util.Success(v) => v
      case scala.util.Failure(ex) => throw ex
    }

  def handleEither[T](thisTry: Either[io.circe.Error, T]): Result[T] =
    thisTry match {
      case Right(v) => Success(v)
      case Left(ex) => Failure(ex)
    }

  def thisType:VariableType

  def getExpression(params:Map[String,Parameter], names:String*):Expression = getParameter(params, names:_*).map(getExpression) match {
    case Some(expr) => expr
    case None => throw new RuntimeException(s"parameter $name not found. available parameters: ${params.keys.mkString(",")}")
  }

  def getParameter(params:Map[String,Parameter], names:String*):Option[Parameter] =
    names.flatMap(params.get).headOption

  def getExpression(param:Parameter):Expression = param match {
    case OneValueParameter(expr) => expr
    case _ => throw new RuntimeException("invalid parameter type " + param.getClass.getSimpleName + " expecting single expression")
  }

  def format(formatter:Option[FormatterDefinition], value:OpenlawValue, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    formatter
      .map(getFormatter(_, executionResult))
      .getOrElse(Success(defaultFormatter))
      .flatMap(_.format(value, executionResult))

  def getMandatoryParameter(name:String, parameter:Parameters):Parameter = {
    parameter.parameterMap.toMap.get(name) match {
      case Some(param) => param
      case None => throw new RuntimeException(s"mandatory parameter $name could not be found")
    }
  }
}

object VariableType {

  def allTypes():Seq[VariableType] = Seq(
    AbstractCollectionType,
    OLOwnType,
    AddressType,
    ChoiceType,
    ClauseType,
    DateType,
    DateTimeType,
    EthAddressType,
    EthTxHashType,
    EthereumCallType,
    EthereumEventFilterType,
    IdentityType,
    LargeTextType,
    ImageType,
    NumberType,
    PeriodType,
    SectionType,
    SmartContractMetadataType,
    AbstractStructureType,
    TemplateType,
    TextType,
    ValidationType,
    YesNoType)

  def getByName(name: String): Option[VariableType] =
    allTypes().find(_.name === name)

  implicit val eqForVariableType: Eq[VariableType] =
    (x: VariableType, y: VariableType) => x == y

  def getPeriod(v: Expression, executionResult: TemplateExecutionResult): Result[Period] =
    get(v,executionResult, PeriodType.cast)
  def getEthereumAddress(v: Expression, executionResult: TemplateExecutionResult): Result[EthereumAddress] =
    get(v,executionResult,EthAddressType.cast)
  def getDate(v: Expression, executionResult: TemplateExecutionResult): Result[OpenlawDateTime] =
    get(v,executionResult, DateTimeType.cast)
  def getMetadata(v: Expression, executionResult: TemplateExecutionResult): Result[SmartContractMetadata] =
    get(v,executionResult,SmartContractMetadataType.cast)
  def getString(v: Expression, executionResult: TemplateExecutionResult): Result[String] =
    get[OpenlawString](v,executionResult, (str,_) => Success(str)).map(_.underlying)

  def get[T <: OpenlawValue](expr: Expression, executionResult: TemplateExecutionResult, cast: (String,TemplateExecutionResult) => Result[T])(implicit classTag: ClassTag[T]): Result[T] =
    expr
      .evaluate(executionResult)
      .flatMap {
        case Some(value:T) => Success(value)
        case Some(value:OpenlawString) => cast(value, executionResult)
        case Some(value) => Failure("cannot get value of type " + value.getClass.getSimpleName + ". expecting " + classTag.runtimeClass.getSimpleName)
        case None => Failure("could not get the value. Missing data")
      }

  def convert[U <: OpenlawValue](value:OpenlawValue)(implicit classTag: ClassTag[U]): Result[U#T] = value match {
    case convertedValue: U =>
      Success(convertedValue.underlying)
    case other =>
      val msg = "invalid type " +
        other.getClass.getSimpleName +
        " expecting " +
        classTag.runtimeClass.getSimpleName +
        s".value:$other"
      Failure(msg)
  }

  def sequence[L,R](seq:Seq[Either[L,R]]):Either[L, Seq[R]] = seq.partition(_.isLeft) match {
    case (Nil,  values) => Right(for(Right(i) <- values.view) yield i)
    case (errs, _) => errs.headOption match {
      case Some(Left(err)) => Left(err)
      case _ => Right(Seq())
    }
  }

  implicit val variableTypeEnc: Encoder[VariableType] = (a: VariableType) =>
    a.serialize

  implicit val variableTypeDec: Decoder[VariableType] = (c: HCursor) => c.downField("name").as[String]
    .flatMap(name => VariableType.allTypes().find(_.name === name) match {
        case Some(varType) => Right(varType)
        case None => createCustomType(c, name)
      })

  private def createCustomType(cursor: HCursor, name:String):Decoder.Result[VariableType] = {
    DefinedStructureType.definedStructureTypeDec(cursor) match {
      case Right(value) => Right(value)
      case Left(_) => DefinedChoiceType.definedChoiceTypeDec(cursor) match {
        case Right(value) => Right(value)
        case Left(_) => Left(DecodingFailure(s"unknown type $name. or error while decoding", List()))
      }
    }
  }
}

object LocalDateTimeHelper {
  implicit val dateDecoder: Decoder[LocalDateTime] = (c: HCursor) => {
    for {
      epoch <- c.as[Long]
    } yield LocalDateTime.ofEpochSecond(epoch, 0,ZoneOffset.UTC)
  }
  implicit val dateEncoder: Encoder[Clock] = (a: Clock) => Json.fromString(a.getZone.getId)

  implicit val clockDecoder: Decoder[Clock] = (c: HCursor) => {
    for {
      zoneId <- c.as[String]
    } yield Clock.system(ZoneId.of(zoneId))
  }
  implicit val clockEncoder: Encoder[LocalDateTime] = (a: LocalDateTime) => Json.fromLong(a.toEpochSecond(ZoneOffset.UTC))
  implicit val eqForLocalDateTime: Eq[LocalDateTime] = Eq.fromUniversalEquals

  implicit val dateKeyEncoder:KeyEncoder[LocalDateTime] = (key: LocalDateTime) => key.toEpochSecond(ZoneOffset.UTC).toString

  implicit val dateKeyDecoder:KeyDecoder[LocalDateTime] = (key: String) => Try(LocalDateTime.ofEpochSecond(key.toLong, 0, ZoneOffset.UTC)).toOption
}
