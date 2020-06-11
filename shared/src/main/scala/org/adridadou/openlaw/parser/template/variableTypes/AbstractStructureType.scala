package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe.{Decoder, Encoder, HCursor, Json}
import cats.kernel.Eq
import io.circe.Json.JString
import org.adridadou.openlaw.parser.template._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{
  Formatter,
  NoopFormatter
}
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}

object Structure {
  implicit val structureEnc: Encoder[Structure] = deriveEncoder
  implicit val structureDec: Decoder[Structure] = deriveDecoder
  implicit val structureDecEq: Eq[Structure] = Eq.fromUniversalEquals
}

final case class Structure(
    typeDefinition: Map[VariableName, VariableDefinition],
    names: List[VariableName],
    types: Map[VariableName, VariableType]
) extends OpenlawNativeValue

case object AbstractStructureType
    extends VariableType(name = "Structure")
    with TypeGenerator[Structure] {
  override def construct(
      param: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[Option[Structure]] = param match {
    case Parameters(values) =>
      val someTest = values
        .foldLeft(Success(Map[VariableName, VariableDefinition]()))({
          case (Success(m), (key, value)) =>
            getField(key, value, executionResult).map(variableDefinition =>
              m + (VariableName(key) -> variableDefinition)
            )
          case (other, (_, _)) =>
            other
        })

      someTest
        .map(fields => {
          val types = fields.map({
            case (key, definition) => key -> definition.varType(executionResult)
          })

          Some(
            Structure(
              types = types,
              typeDefinition = fields,
              names = fields.keys.toList
            )
          )
        })
    case parameter =>
      Failure(
        s"structure must have one or more expressions as constructor parameters, instead received ${parameter.getClass}"
      )
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[Structure] = Failure("structured type definition cannot be casted")

  override def internalFormat(value: OpenlawValue): Result[String] =
    Failure("no internal format for structured type definition")

  override def getTypeClass: Class[_ <: Structure] = classOf[Structure]

  def thisType: VariableType = AbstractStructureType

  override def generateType(
      name: VariableName,
      structure: Structure
  ): DefinedStructureType =
    DefinedStructureType(structure, name.name)

  private def getField(
      name: String,
      value: Parameter,
      executionResult: TemplateExecutionResult
  ): Result[VariableDefinition] = value match {
    case OneValueParameter(VariableName(typeName)) =>
      Success(
        VariableDefinition(
          name = VariableName(name),
          variableTypeDefinition = Some(VariableTypeDefinition(name = typeName))
        )
      )
    case OneValueParameter(definition: VariableDefinition) =>
      Success(definition.copy(name = VariableName(name), variableTypeDefinition = definition.variableTypeDefinition))
    case _ =>
      Failure("error in the constructor for Structured Type")
  }
}

object DefinedStructureType {
  implicit val definedStructureTypeEnc: Encoder[DefinedStructureType] =
    (a: DefinedStructureType) => a.serialize
  implicit val definedStructureTypeDec: Decoder[DefinedStructureType] =
    (c: HCursor) =>
      for {
        name <- c.downField("name").as[String]
        structure <- c.downField("structure").as[Structure]
      } yield DefinedStructureType(structure = structure, typeName = name)

}

final case class DefinedStructureType(structure: Structure, typeName: String)
    extends VariableType(name = typeName) {

  override def serialize: Json =
    Json.obj(
      "name" -> Json.fromString(typeName),
      "structure" -> structure.asJson
    )

  override def defaultFormatter: Formatter = new NoopFormatter

  override def access(
      value: OpenlawValue,
      name: VariableName,
      keys: List[VariableMemberKey],
      executionResult: TemplateExecutionResult
  ): Result[Option[OpenlawValue]] = {
    keys match {
      case Nil =>
        Success(Some(value))
      case VariableMemberKey(Left(head)) :: tail =>
        VariableType
          .convert[OpenlawMap[VariableName, OpenlawValue]](value)
          .flatMap { values =>
            (for {
              result <- values.get(head)
              keyType <- structure.typeDefinition.get(head)
            } yield keyType
              .varType(executionResult)
              .access(result, name, tail, executionResult)) match {
              case Some(result) => result
              case None if structure.names.contains(head) =>
                Success(None)
              case None =>
                Failure(
                  s"properties '${keys.mkString(".")}' could not be resolved for the structured type $typeName. available properties ${structure.names
                    .map(name => s"'${name.name}'")
                    .mkString(",")}"
                )
            }
          }
      case _ => Failure("Structure doesn't handle functions yet")
    }
  }

  override def getTypeClass: Class[OpenlawMap[VariableName, OpenlawValue]] =
    classOf[OpenlawMap[VariableName, OpenlawValue]]

  override def keysType(
      keys: List[VariableMemberKey],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[VariableType] =
    keys match {
      case Nil =>
        Success(AbstractStructureType)
      case VariableMemberKey(Left(name)) :: tail =>
        structure.typeDefinition.get(name) match {
          case Some(varDefinition) =>
            varDefinition
              .varType(executionResult)
              .keysType(tail, expression, executionResult)
          case None =>
            Failure(
              s"property '${keys.mkString(".")}' could not be resolved in structure value '$name'"
            )
        }
      case _ => Failure("Structure doesn't handle functions yet")
    }

  override def validateKeys(
      name: VariableName,
      keys: List[VariableMemberKey],
      expression: Expression,
      executionResult: TemplateExecutionResult
  ): Result[Unit] = keys match {
    case Nil =>
      Success.unit
    case VariableMemberKey(Left(name)) :: tail =>
      structure.typeDefinition.get(name).map(_.varType(executionResult)) match {
        case Some(variableType: NoShowInForm) =>
          Failure(
            s"invalid type in structure ${variableType.name} only types that should be shown in the input form are allowed (Text, YesNo, Address ...)"
          )
        case Some(variableType) =>
          variableType.validateKeys(name, tail, expression, executionResult)
        case None =>
          Failure(
            s"property '${tail.mkString(".")}' could not be resolved in structure value '$name'"
          )
      }
    case _ => Failure("Structure doesn't handle functions yet")
  }

  override def cast(
      value: String,
      executionResult: TemplateExecutionResult
  ): Result[OpenlawMap[VariableName, OpenlawValue]] =
    for {
      values <- decode[Map[String, Json]](value).leftMap(FailureException(_))
      list <- structure.typeDefinition
        .flatMap {
          case (fieldName, fieldType) =>
            values
              .get(fieldName.name)
              .map(value =>
                fieldType
                  .cast(jsonToString(value), executionResult)
                  .map(fieldName -> _)
              )
        }
        .toList
        .sequence
    } yield OpenlawMap(list.toMap)

  private def jsonToString(json: Json): String = {
    json.fold[String](
      "null",
      bool => bool.toString,
      number => number.toString,
      string => string,
      array => array.asJson.noSpaces,
      jsonObject => jsonObject.asJson.noSpaces
    )
  }

  override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType
      .convert[OpenlawMap[VariableName, OpenlawValue]](value)
      .flatMap { values =>
        structure.types
          .flatMap {
            case (fieldName, fieldType) =>
              values
                .get(fieldName)
                .map(value =>
                  fieldType.internalFormat(value).map(fieldName.name -> _)
                )
          }
          .toList
          .sequence
          .map(_.toMap)
          .map { _.asJson.noSpaces }
      }

  override def thisType: VariableType = this
}
