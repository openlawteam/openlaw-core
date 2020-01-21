package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import cats.implicits._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.{TemplateExecutionResult, VariableMemberKey, VariableName}
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}

case object AbstractCollectionType extends VariableType("Collection") with ParameterTypeProvider {

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OpenlawValue] =
    Failure("abstract collection! Should use a concrete definition of this type")

  override def internalFormat(value: OpenlawValue): Result[String] =
    Failure("abstract collection! Should use a concrete definition of this type")

  override def thisType: VariableType =
    this

  override def getTypeClass: Class[OpenlawValue] = classOf[OpenlawValue]

  override def createParameterInstance(typeParameter: VariableType): CollectionType =
    CollectionType(typeParameter)
}

final case class CollectionValue(size:Int = 1, values:Map[Int, OpenlawValue] = Map(), collectionType:CollectionType) extends OpenlawNativeValue {
  def castValue(value:String, executionResult: TemplateExecutionResult):Result[OpenlawValue] = collectionType.typeParameter.cast(value, executionResult)
  def valueInternalFormat(value:OpenlawValue): Result[String] = collectionType.typeParameter.internalFormat(value)
  def list:List[OpenlawValue] = values.values.toList
}

object CollectionTypeValue {
  implicit val collectionTypeValueEnc:Encoder[CollectionTypeValue] = deriveEncoder
  implicit val collectionTypeValueDec:Decoder[CollectionTypeValue] = deriveDecoder
}

final case class CollectionTypeValue(values:Map[Int, String], size:Int)

final case class CollectionType(typeParameter:VariableType) extends VariableType("Collection") with ParameterType {

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[CollectionValue] = for {
      collectionValue <- decode[CollectionTypeValue](value).leftMap(FailureException(_))
      values <- collectionValue.values.map { case (key, v) => typeParameter.cast(v, executionResult).map(key -> _) }
        .toList
        .sequence
        .map(_.toMap)
    } yield CollectionValue(size = collectionValue.size, values = values, collectionType = this)


  override def defaultFormatter: Formatter = new NoopFormatter

  override def internalFormat(value: OpenlawValue): Result[String] = {
    for {
      collection <- VariableType.convert[CollectionValue](value)
      values <- collection.values.map({ case (key, v) => typeParameter.internalFormat(v).map(p => key -> p) }).toList.sequence
    } yield CollectionTypeValue(values = values.toMap, size = collection.size).asJson.noSpaces
  }

  override def getTypeClass: Class[_ <: CollectionValue] = classOf[CollectionValue]

  override def thisType: VariableType = this

  override def access(value: OpenlawValue, name:VariableName, keys: List[VariableMemberKey], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    keys match {
      case Nil =>
        Success(Some(value))
      case VariableMemberKey(Right(OLFunctionCall(VariableName("map"), func:OLFunction))) :: Nil =>
        for {
          collectionValue <- VariableType.convert[CollectionValue](value)
          newValues <- collectionValue.values.map({case (index, value) => applyMap(value, func, executionResult).map(index -> _)}).toList.sequence.map(_.toMap)
        } yield {
          val cleanedNewValues = newValues.flatMap({case (index, optValue) => optValue.map(index -> _)})
          Some(collectionValue.copy(values = cleanedNewValues, size = cleanedNewValues.size))
        }
      case _ =>
        super.access(value, name, keys, executionResult)
    }
  }

  private def applyMap(value:OpenlawValue, func:OLFunction, executionResult: TemplateExecutionResult):Result[Option[OpenlawValue]] = for {
    ier <- executionResult.withVariable(func.parameter.name, value, func.parameter.varType(executionResult))
    newValue <- func.expression.evaluate(ier)
  } yield newValue

  override def keysType(keys: List[VariableMemberKey], expression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] =
    keys match {
      case Nil =>
        Success(AbstractStructureType)
      case VariableMemberKey(Right(OLFunctionCall(VariableName("map"), _)))::Nil =>
        Success(AbstractFunctionType)
      case _ =>
        Failure(s"property '${keys.mkString(".")}' could not be resolved in collection type")
    }

  override def validateKeys(name:VariableName, keys: List[VariableMemberKey], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] = keys match {
    case Nil =>
      Success.unit
    case VariableMemberKey(Right(OLFunctionCall(VariableName("map"), func:OLFunction)))::Nil =>
      Success.unit
    case _ =>
      Failure(s"property '${keys.mkString(".")}' could not be resolved in collection type")
  }

}
