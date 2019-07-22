package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder}
import cats.implicits._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result}

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

case class CollectionValue(size:Int = 1, values:Map[Int, OpenlawValue] = Map(), collectionType:CollectionType) extends OpenlawNativeValue {
  def castValue(value:String, executionResult: TemplateExecutionResult):Result[OpenlawValue] = collectionType.typeParameter.cast(value, executionResult)
  def valueInternalFormat(value:OpenlawValue): Result[String] = collectionType.typeParameter.internalFormat(value)
  def list:Seq[OpenlawValue] = values.values.toSeq
}

object CollectionTypeValue {
  implicit val collectionTypeValueEnc:Encoder[CollectionTypeValue] = deriveEncoder
  implicit val collectionTypeValueDec:Decoder[CollectionTypeValue] = deriveDecoder
}

case class CollectionTypeValue(values:Map[Int, String], size:Int)

case class CollectionType(typeParameter:VariableType) extends VariableType("Collection") with ParameterType {

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[CollectionValue] =
    for {
      collectionValue <- handleEither(decode[CollectionTypeValue](value))
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
}