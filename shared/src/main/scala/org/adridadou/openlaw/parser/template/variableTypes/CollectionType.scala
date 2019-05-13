package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.OpenlawValue
import org.adridadou.openlaw.parser.template.TemplateExecutionResult
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import play.api.libs.json.{JsObject, JsString, Json}


case object AbstractCollectionType extends VariableType("Collection") with ParameterTypeProvider {

  override def cast(value: String, executionResult: TemplateExecutionResult): OpenlawValue =
    throw new RuntimeException("abstract collection! Should use a concrete definition of this type")

  override def internalFormat(value: OpenlawValue): String =
    throw new RuntimeException("abstract collection! Should use a concrete definition of this type")

  override def thisType: VariableType =
    this

  override def getTypeClass: Class[_ <: AbstractCollectionType.type ] = this.getClass

  override def createParameterInstance(typeParameter: VariableType): CollectionType =
    CollectionType(typeParameter)
}

case class CollectionValue(size:Int = 1, values:Map[Int, OpenlawValue] = Map(), collectionType:CollectionType) extends OpenlawValue {
  def castValue(value:String, executionResult: TemplateExecutionResult):Any = collectionType.typeParameter.cast(value, executionResult)
  def valueInternalFormat(value:OpenlawValue):String = collectionType.typeParameter.internalFormat(value)
  def list:Seq[OpenlawValue] = values.values.toSeq
}

case class CollectionType(typeParameter:VariableType) extends VariableType("Collection") with ParameterType {

  override def cast(value: String, executionResult: TemplateExecutionResult): CollectionValue = {
    val obj = Json.parse(value).as[JsObject]
      val values = obj("values").as[Map[String, String]]
        .map({case (key, v) => key.toInt -> typeParameter.cast(v, executionResult)})
      val size = obj("size").as[Int]

    CollectionValue(size = size, values = values, collectionType = this)
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def internalFormat(value: OpenlawValue): String = {
    val collection = VariableType.convert[CollectionValue](value)
    Json.obj(
      "values" -> JsObject(collection.values.map({case (key,v) => key.toString -> JsString(typeParameter.internalFormat(v))})),
      "size" -> collection.size
    ).toString()
  }

  override def getTypeClass: Class[_ <: CollectionValue] = classOf[CollectionValue]

  override def thisType: VariableType = this
}