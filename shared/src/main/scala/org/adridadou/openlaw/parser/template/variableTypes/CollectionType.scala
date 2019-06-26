package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result}
import play.api.libs.json.{JsObject, JsString, Json}

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
  def castValue(value:String, executionResult: TemplateExecutionResult):Any = collectionType.typeParameter.cast(value, executionResult)
  def valueInternalFormat(value:OpenlawValue): Result[String] = collectionType.typeParameter.internalFormat(value)
  def list:Seq[OpenlawValue] = values.values.toSeq
}

case class CollectionType(typeParameter:VariableType) extends VariableType("Collection") with ParameterType {

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[CollectionValue] = {
    val obj = Json.parse(value).as[JsObject]
    val valuesResult =
      obj("values")
      .as[Map[String, String]]
      .map { case (key, v) => typeParameter.cast(v, executionResult).map(key.toInt -> _) }
      .toList
      .sequence
      .map(_.toMap)
    val size = obj("size").as[Int]

    valuesResult.map { values => CollectionValue(size = size, values = values, collectionType = this) }
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def internalFormat(value: OpenlawValue): Result[String] = {
    for {
      collection <- VariableType.convert[CollectionValue](value)
      values <- collection.values.map({ case (key, v) => typeParameter.internalFormat(v).map(p => key.toString -> JsString(p)) }).toList.sequence
    } yield {
      Json.obj(
        "values" -> JsObject(values),
        "size" -> collection.size
      ).toString()
    }
  }

  override def getTypeClass: Class[_ <: CollectionValue] = classOf[CollectionValue]

  override def thisType: VariableType = this
}