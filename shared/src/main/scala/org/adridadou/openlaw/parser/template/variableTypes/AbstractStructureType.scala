package org.adridadou.openlaw.parser.template.variableTypes

import org.adridadou.openlaw.parser.template._
import play.api.libs.json.{JsObject, Json}
import io.circe.syntax._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}

import scala.util.Try

case class Structure(typeDefinition: Map[VariableName, VariableType], names:Seq[VariableName])

case object AbstractStructureType extends VariableType(name = "Structure") with TypeGenerator[Structure] {
  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Either[Throwable, Option[Structure]] = param match {
    case Parameters(values) =>
      Try(Some(
        Structure(typeDefinition = values
          .flatMap({case (key,value) => getField(key,value, executionResult)}).toMap, names = values.map({case (key,_) => VariableName(key)}))
      )).toEither
    case parameter =>
      Left(new Exception(s"structure must have one or more expressions as constructor parameters, instead received ${parameter.getClass}"))
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Structure = throw new RuntimeException("structured type definition cannot be casted")

  override def internalFormat(value: Any): String = throw new RuntimeException("no internal format for structured type definition")

  override def getTypeClass: Class[_ <: AbstractStructureType.type] = this.getClass

  override def checkTypeName(nameToCheck: String): Boolean = Seq("Structure").exists(_.equalsIgnoreCase(nameToCheck))

  def thisType: VariableType = AbstractStructureType

  override def generateType(name: VariableName, structure: Structure): VariableType =
    DefinedStructureType(structure, name.name)

  private def getField(name:String, value:Parameter, executionResult: TemplateExecutionResult): Option[(VariableName, VariableType)] = value match {
    case OneValueParameter(VariableName(typeName)) => executionResult.findVariableType(VariableTypeDefinition(typeName)).map(VariableName(name) -> _)
    case _ => throw new RuntimeException("error in the constructor for Structured Type")
  }
}

case class DefinedStructureType(structure:Structure, typeName:String) extends VariableType(name = typeName) {

  override def defaultFormatter: Formatter = new NoopFormatter

  override def access(value: Any, keys: Seq[String], executionResult: TemplateExecutionResult): Either[String, Any] = {
    keys.toList match {
      case Nil =>
        Right(value)
      case head :: tail =>
        val name = VariableName(head)
        val values = VariableType.convert[Map[VariableName, Any]](value)
        (for {
          result <- values.get(name)
          keyType <- structure.typeDefinition.get(name)
        } yield keyType.access(result, tail, executionResult) ) match {
          case Some(result) => result
          case None if structure.names.contains(name) =>
            Right(structure.typeDefinition(name).missingValueFormat(name))
          case None =>
            Left(s"properties '${keys.mkString(".")}' could not be resolved for the structured type $typeName. available properties ${structure.names.map(name => s"'${name.name}'").mkString(",")}")
        }
    }
  }

  override def getTypeClass: Class[Map[VariableName, Any]] = classOf[Map[VariableName, Any]]

  override def keysType(keys: Seq[String], executionResult: TemplateExecutionResult): VariableType = {
    keys.toList match {
      case Nil =>
        AbstractStructureType
      case head::tail =>
        val name = VariableName(head)
        structure.typeDefinition.get(name) match {
          case Some(varType) =>
            varType.keysType(tail, executionResult)
          case None =>
            throw new RuntimeException(s"property '${keys.mkString(".")}' could not be resolved in structure value '$head'")
        }
    }
  }

  override def validateKeys(name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Option[String] = keys.toList match {
    case Nil =>
      None
    case head::tail =>
      val name = VariableName(head)
      structure.typeDefinition.get(name) match {
        case Some(variableType:NoShowInForm) =>
          Some(s"invalid type in structure ${variableType.name} only types that should be shown in the input form are allowed (Text, YesNo, Address ...)")
        case Some(variableType) =>
          variableType.validateKeys(name, tail, executionResult)
        case None =>
          Some(s"property '${tail.mkString(".")}' could not be resolved in structure value '$head'")
      }
  }

  override def cast(value: String, executionResult: TemplateExecutionResult): Map[VariableName, Any] = {
    val json = Json.parse(value)

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

