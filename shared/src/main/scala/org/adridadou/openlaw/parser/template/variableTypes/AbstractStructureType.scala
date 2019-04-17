package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder, HCursor, Json}
import org.adridadou.openlaw.parser.template._
import play.api.libs.json.JsObject
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success}


object Structure {
  implicit val structureEnc:Encoder[Structure] = deriveEncoder[Structure]
  implicit val structureDec:Decoder[Structure] = deriveDecoder[Structure]
}


case class Structure(typeDefinition: Map[VariableName, VariableType], names:Seq[VariableName])

case object AbstractStructureType extends VariableType(name = "Structure") with TypeGenerator[Structure] {
  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Result[Option[Structure]] = param match {
    case Parameters(values) =>
      VariableType.sequence(values
        .map({case (key,value) => getField(key,value, executionResult)})).map(_.flatten)
        .map(fieldMap => Some(
            Structure(typeDefinition = fieldMap.toMap, names = values.map({case (key,_) => VariableName(key)}))
          ))
    case parameter =>
      Failure(s"structure must have one or more expressions as constructor parameters, instead received ${parameter.getClass}")
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Structure = throw new RuntimeException("structured type definition cannot be casted")

  override def internalFormat(value: Any): String = throw new RuntimeException("no internal format for structured type definition")

  override def getTypeClass: Class[_ <: AbstractStructureType.type] = this.getClass

  override def checkTypeName(nameToCheck: String): Boolean = Seq("Structure").exists(_.equalsIgnoreCase(nameToCheck))

  def thisType: VariableType = AbstractStructureType

  override def generateType(name: VariableName, structure: Structure): VariableType =
    DefinedStructureType(structure, name.name)

  private def getField(name:String, value:Parameter, executionResult: TemplateExecutionResult): Result[Option[(VariableName, VariableType)]] = value match {
    case OneValueParameter(VariableName(typeName)) =>
      Success(executionResult
        .findVariableType(VariableTypeDefinition(typeName))
        .map(VariableName(name) -> _))
    case _ =>
      Failure("error in the constructor for Structured Type")
  }
}

object DefinedStructureType {
  implicit val definedStructureTypeEnc:Encoder[DefinedStructureType] = (a: DefinedStructureType) => a.serialize
  implicit val definedStructureTypeDec:Decoder[DefinedStructureType] = (c:HCursor) => for {
    name <- c.downField("name").as[String]
    structure <- c.downField("structure").as[Structure]
  } yield DefinedStructureType(structure = structure, typeName = name)

}

case class DefinedStructureType(structure:Structure, typeName:String) extends VariableType(name = typeName) {


  override def serialize: Json = {
    Json.obj(
      "name" -> Json.fromString(typeName),
      "structure" -> structure.asJson
    )
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def access(value: Any, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Any] = {
    keys.toList match {
      case Nil =>
        Success(value)
      case head :: tail =>
        val headName = VariableName(head)
        val values = VariableType.convert[Map[VariableName, Any]](value)
        (for {
          result <- values.get(headName)
          keyType <- structure.typeDefinition.get(headName)
        } yield keyType.access(result, name, tail, executionResult) ) match {
          case Some(result) => result
          case None if structure.names.contains(headName) =>
            Success(structure.typeDefinition(headName).missingValueFormat(headName))
          case None =>
            Failure(s"properties '${keys.mkString(".")}' could not be resolved for the structured type $typeName. available properties ${structure.names.map(name => s"'${name.name}'").mkString(",")}")
        }
    }
  }

  override def getTypeClass: Class[Map[VariableName, Any]] = classOf[Map[VariableName, Any]]

  override def keysType(keys: Seq[String], expression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] = {
    keys.toList match {
      case Nil =>
        Success(AbstractStructureType)
      case head::tail =>
        val name = VariableName(head)
        structure.typeDefinition.get(name) match {
          case Some(varType) =>
            varType.keysType(keys, expression, executionResult)
          case None =>
            throw new RuntimeException(s"property '${keys.mkString(".")}' could not be resolved in structure value '$head'")
        }
    }
  }

  override def validateKeys(name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Unit] = keys.toList match {
    case Nil =>
      Success(())
    case head::tail =>
      val name = VariableName(head)
      structure.typeDefinition.get(name) match {
        case Some(variableType:NoShowInForm) =>
          Failure(s"invalid type in structure ${variableType.name} only types that should be shown in the input form are allowed (Text, YesNo, Address ...)")
        case Some(variableType) =>
          variableType.validateKeys(name, tail, executionResult)
        case None =>
          Failure(s"property '${tail.mkString(".")}' could not be resolved in structure value '$head'")
      }
  }

  override def cast(value: String, executionResult: TemplateExecutionResult): Map[VariableName, Any] = {
    val json = play.api.libs.json.Json.parse(value)

    structure.typeDefinition.flatMap({case (fieldName, fieldType) =>
          (json.as[JsObject] \ fieldName.name).asOpt[String].map(value => fieldName -> fieldType.cast(value, executionResult))
    })
  }

  override def internalFormat(value: Any): String = {
    val values = VariableType.convert[Map[VariableName, Any]](value)
    structure.typeDefinition
      .flatMap({case (fieldName,fieldType) => values.get(fieldName).map(value => fieldName.name -> fieldType.internalFormat(value))})
      .asJson.noSpaces
  }

  override def thisType: VariableType = this
}

