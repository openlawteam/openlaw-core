package org.adridadou.openlaw.parser.template.variableTypes

import java.time.{Clock, LocalDateTime, ZoneId, ZoneOffset}

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.expressions.{Expression, ValueExpression}
import org.adridadou.openlaw.parser.template.formatters.{DefaultFormatter, Formatter}
import cats.Eq
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe._

import scala.reflect.ClassTag
import scala.util.Try
import org.adridadou.openlaw.result.{Failure, Result, Success, attempt}
import LocalDateTimeHelper._

trait NoShowInForm

trait ActionValue {
  def nextActionSchedule(executionResult: TemplateExecutionResult, pastExecutions:Seq[OpenlawExecution]):Option[LocalDateTime]
}

trait ActionType extends NoShowInForm {
  def actionValue(value:Any):ActionValue
}

trait OpenlawExecution {
  def scheduledDate:LocalDateTime
  def executionDate:LocalDateTime
  def executionStatus:OpenlawExecutionStatus
  def key:Any
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

  def accessVariables(name:VariableName, keys:Seq[String], executionResult: TemplateExecutionResult): Seq[VariableName] =
    Seq(name)

  def operationWith(rightType: VariableType, operation: ValueOperation): VariableType =
    this

  def access(value: Any, keys: Seq[String], executionResult:TemplateExecutionResult): Result[Any] = {
    if(keys.isEmpty) {
      Success(value)
    } else {
      Failure(s"The variable type $name has no properties")
    }
  }

  def getTypeClass: Class[_]

  def checkTypeName(nameToCheck: String): Boolean =
    this.name.equalsIgnoreCase(nameToCheck)

  def validateKeys(name:VariableName, keys:Seq[String], executionResult: TemplateExecutionResult): Result[Unit] =
    keys.headOption.map(_ => Failure(s"The variable type $name has no properties")).getOrElse(Success(()))

  def keysType(keys: Seq[String], definition: VariableDefinition, executionResult: TemplateExecutionResult):Result[VariableType] = if(keys.nonEmpty) {
    Failure(s"the type $name has no properties")
  } else {
    Success(thisType)
  }

  def plus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Any] =
    throw new UnsupportedOperationException(s"$name type does not support addition")
  def minus(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Any] =
    throw new UnsupportedOperationException(s"$name type does not support substraction")
  def multiply(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Any] =
    throw new UnsupportedOperationException(s"$name type does not support multiplication")
  def divide(optLeft: Option[Any], optRight: Option[Any], executionResult: TemplateExecutionResult): Option[Any] =
    throw new UnsupportedOperationException(s"$name type does not support division")

  def isCompatibleType(otherType: VariableType, operation: ValueOperation): Boolean =
    otherType === this

  def cast(value: String, executionResult:TemplateExecutionResult): Any

  def missingValueFormat(name: VariableName): Seq[AgreementElement] = Seq(FreeText(Text(s"[[${name.name}]]")))

  def internalFormat(value: Any): String

  def construct(constructorParams: Parameter, executionResult: TemplateExecutionResult): Result[Option[Any]] = constructorParams match {
      case OneValueParameter(expr) =>
        attempt(expr.evaluate(executionResult))
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

  def getFormatter(name:FormatterDefinition, executionResult:TemplateExecutionResult):Formatter =
    defaultFormatter

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

  def handleEither[T](thisTry: Either[io.circe.Error, T]): T =
    thisTry match {
      case Right(v) => v
      case Left(ex) => throw new RuntimeException(ex)
    }

  def thisType:VariableType

  def getExpression(params:Map[String,Parameter], name:String):Expression = params.get(name).map(getExpression) match {
    case Some(expr) => expr
    case None => throw new RuntimeException(s"parameter $name not found. available parameters: ${params.keys.mkString(",")}")
  }

  def getExpression(param:Parameter):Expression = param match {
    case OneValueParameter(expr) => expr
    case _ => throw new RuntimeException("invalid parameter type " + param.getClass.getSimpleName + " expecting single expression")
  }

  def format(formatter:Option[FormatterDefinition], value:Any, executionResult: TemplateExecutionResult): Result[Seq[AgreementElement]] =
    formatter
      .map(getFormatter(_, executionResult))
      .getOrElse(defaultFormatter)
      .format(value, executionResult)

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
    AddressType,
    ChoiceType,
    ClauseType,
    DateType,
    DateTimeType,
    EthAddressType,
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

  def getPeriod(v: Expression, executionResult: TemplateExecutionResult): Period =
    get(v,executionResult, PeriodType.cast)
  def getEthereumAddress(v: Expression, executionResult: TemplateExecutionResult): EthereumAddress =
    get(v,executionResult,EthAddressType.cast)
  def getDate(v: Expression, executionResult: TemplateExecutionResult): LocalDateTime =
    get(v,executionResult, DateTimeType.cast)
  def getMetadata(v: Expression, executionResult: TemplateExecutionResult): SmartContractMetadata =
    get(v,executionResult,SmartContractMetadataType.cast)
  def getString(v: Expression, executionResult: TemplateExecutionResult): String =
    get(v,executionResult, (str,_) => str)

  def get[T](v: Expression, executionResult: TemplateExecutionResult, cast: (String,TemplateExecutionResult) => T)(implicit classTag: ClassTag[T]): T =
    v.evaluate(executionResult).map({
      case value:T => value
      case value:String => cast(value, executionResult)
      case value => throw new RuntimeException("cannot get value of type " + value.getClass.getSimpleName + ". expecting " + classTag.runtimeClass.getSimpleName)
    }) match {
      case Some(value) => value
      case None => throw new RuntimeException("could not get the value. Missing data")
    }

  def convert[T](value:Any)(implicit classTag: ClassTag[T]): T = value match {
    case convertedValue: T =>
      convertedValue
    case other =>
      val msg = "invalid type " +
        other.getClass.getSimpleName +
        " expecting " +
        classTag.runtimeClass.getSimpleName +
        s".value:$other"
      throw new RuntimeException(msg)
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

}
