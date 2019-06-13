package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.adridadou.openlaw.parser.template._
import play.api.libs.json.JsObject
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success}


object Structure {
  implicit val structureEnc:Encoder[Structure] = deriveEncoder[Structure]
  implicit val structureDec:Decoder[Structure] = deriveDecoder[Structure]
}


case class Structure(typeDefinition: Map[VariableName, VariableDefinition], names:Seq[VariableName], types:Map[VariableName, VariableType]) extends OpenlawNativeValue

case object AbstractStructureType extends VariableType(name = "Structure") with TypeGenerator[Structure] {
  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Result[Option[Structure]] = param match {
    case Parameters(values) =>
      VariableType.sequence(values
        .map({case (key,value) => getField(key, value, executionResult).map(VariableName(key) -> _)}))
        .map(fields => {
          val types = fields.map({case (key,definition) => key -> definition.varType(executionResult)})
          Some(
            Structure(types = types.toMap, typeDefinition = fields.toMap, names = values.map({case (key,_) => VariableName(key)}))
          )
        })
    case parameter =>
      Failure(s"structure must have one or more expressions as constructor parameters, instead received ${parameter.getClass}")
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[Structure] = Failure("structured type definition cannot be casted")

  override def internalFormat(value: OpenlawValue): Result[String] = Failure("no internal format for structured type definition")

  override def getTypeClass: Class[_ <: Structure] = classOf[Structure]

  override def checkTypeName(nameToCheck: String): Boolean = Seq("Structure").exists(_.equalsIgnoreCase(nameToCheck))

  def thisType: VariableType = AbstractStructureType

  override def generateType(name: VariableName, structure: Structure): VariableType =
    DefinedStructureType(structure, name.name)

  private def getField(name:String, value:Parameter, executionResult: TemplateExecutionResult): Result[VariableDefinition] = value match {
    case OneValueParameter(VariableName(typeName)) =>
      Success(VariableDefinition(name = VariableName(name), variableTypeDefinition = Some(VariableTypeDefinition(name = typeName))))
    case OneValueParameter(definition:VariableDefinition) =>
      Success(definition.copy(name = VariableName(name)))
    case param =>
      println(param)
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

  override def access(value: OpenlawValue, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    keys.toList match {
      case Nil =>
        Success(Some(value))
      case head :: tail =>
        val headName = VariableName(head)
        VariableType.convert[OpenlawMap[VariableName, OpenlawValue]](value).flatMap { values =>
          (for {
            result <- values.get(headName)
            keyType <- structure.typeDefinition.get(headName)
          } yield keyType.varType(executionResult).access(result, name, tail, executionResult)) match {
            case Some(result) => result
            case None if structure.names.contains(headName) =>
              Success(Some(structure.typeDefinition(headName).varType(executionResult).missingValueFormat(headName)))
            case None =>
              Failure(s"properties '${keys.mkString(".")}' could not be resolved for the structured type $typeName. available properties ${structure.names.map(name => s"'${name.name}'").mkString(",")}")
          }
        }
    }
  }

  override def getTypeClass: Class[OpenlawMap[VariableName, OpenlawValue]] = classOf[OpenlawMap[VariableName, OpenlawValue]]

  override def keysType(keys: Seq[String], expression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] = {
    keys.toList match {
      case Nil =>
        Success(AbstractStructureType)
      case head::_ =>
        val name = VariableName(head)
        structure.typeDefinition.get(name) match {
          case Some(varDefinition) =>
            varDefinition.varType(executionResult).keysType(keys, expression, executionResult)
          case None =>
            Failure(s"property '${keys.mkString(".")}' could not be resolved in structure value '$head'")
        }
    }
  }

  override def validateKeys(name:VariableName, keys: Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] = keys.toList match {
    case Nil =>
      Success(())
    case head::tail =>
      val name = VariableName(head)
      structure.typeDefinition.get(name) match {
        case Some(variableType:NoShowInForm) =>
          Failure(s"invalid type in structure ${variableType.name} only types that should be shown in the input form are allowed (Text, YesNo, Address ...)")
        case Some(variableType) =>
          variableType.varType(executionResult).validateKeys(name, tail, expression, executionResult)
        case None =>
          Failure(s"property '${tail.mkString(".")}' could not be resolved in structure value '$head'")
      }
  }

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OpenlawMap[VariableName, OpenlawValue]] = {
    val json = play.api.libs.json.Json.parse(value)

    structure.typeDefinition.flatMap {case (fieldName, fieldType) =>
      (json.as[JsObject] \ fieldName.name).asOpt[String].map(value => fieldType.cast(value, executionResult).map(fieldName -> _))
    }
    .toList
    .sequence
    .map(list => OpenlawMap(list.toMap))
  }

  override def internalFormat(value: OpenlawValue): Result[String] = {
    VariableType.convert[OpenlawMap[VariableName, OpenlawValue]](value).flatMap { values =>
      structure
        .types
        .flatMap { case (fieldName, fieldType) => values.get(fieldName).map(value => fieldType.internalFormat(value).map( fieldName.name -> _)) }
        .toList
        .sequence
        .map(_.toMap)
        .map { _.asJson.noSpaces }
    }
  }

  override def thisType: VariableType = this
}

