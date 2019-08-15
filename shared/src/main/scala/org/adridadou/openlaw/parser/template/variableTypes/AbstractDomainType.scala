package org.adridadou.openlaw.parser.template.variableTypes

import cats.implicits._
import io.circe.{Decoder, Encoder, HCursor, Json}
import cats.kernel.Eq
import org.adridadou.openlaw.parser.template._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, FailureException, Result, Success}

case class DomainInformation(variableType:Map[VariableName, VariableType], validation:Validation) extends OpenlawNativeValue

case object DomainInformation {
  implicit val domainEnc:Encoder[DomainInformation] = deriveEncoder[DomainInformation]
  implicit val domainDec:Decoder[DomainInformation] = deriveDecoder[DomainInformation]
  implicit val domainDecEq:Eq[DomainInformation] = Eq.fromUniversalEquals

}

case object AbstractDomainType extends VariableType(name = "Domain") with TypeGenerator[DomainInformation] {

  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Result[Option[DomainInformation]] = param match {
    case Parameters(seq) =>
      val map = seq.toMap
      for {
        variableName <- map.get("variableType")
        validation <- map.get("validation")
      } yield {
        val  = executionResult.getVariable(variableName).map(_.varType(executionResult))
        val validationValue = ValidationType.cast(getOneValueConstant(validation).toString, executionResult).right.get
        DomainInformation(variableTypeValue, validationValue)
      }
    case _ =>
      Failure("""Domain type requires parameters (either 'variableType' or 'validation' is missing)""")
    /*case Parameters(values) =>
      VariableType.sequence(values
        .map(
          {case (key,value) => getField(key, value, executionResult).map(VariableName(key) -> _)}

        ))

        /*.map(fields => {
          val types = fields.map({case (key,definition) => key -> definition.varType(executionResult)})*/
          Some(
            DomainInformation(variableType = values.map({case (key,_) => VariableName(key)}), )
          )
        //}//)
    case parameter =>
      Failure(s"structure must have one or more expressions as constructor parameters, instead received ${parameter.getClass}")*/

    }

  private def getType(typeName:String, value:Parameter, executionResult: TemplateExecutionResult): Result[VariableType] = value match {
    case OneValueParameter(VariableName(typeName)) =>
      Success(VariableType(name = VariableType(typeName)))
    case _ =>
      Failure("error in the constructor for Structured Type")
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[DomainInformation] = Failure("domain type definition cannot be casted")

  override def internalFormat(value: OpenlawValue): Result[String] = Failure("no internal format for domain type definition")

  override def getTypeClass: Class[_ <: DomainInformation] = classOf[DomainInformation]

  override def checkTypeName(nameToCheck: String): Boolean = Seq("DomainInformation").exists(_.equalsIgnoreCase(nameToCheck))

  def thisType: VariableType = AbstractDomainType

  private def getOneValueConstant(value:Parameter): Result[String] = value match {
    case OneValueParameter(StringConstant(v, _)) =>
      Success(v)
    case _ =>
      Failure("""Domain requires "variableType" or "validation" argument.""")
  }

  private def getField(name:String, value:Parameter, executionResult: TemplateExecutionResult): Result[VariableDefinition] = value match {
    case OneValueParameter(VariableName(typeName)) =>
      Success(VariableDefinition(name = VariableName(name), variableTypeDefinition = Some(VariableTypeDefinition(name = typeName))))
    case OneValueParameter(definition:VariableDefinition) =>
      Success(definition.copy(name = VariableName(name)))
    case _ =>
      Failure("error in the constructor for Structured Type")
  }

  override def generateType(name: VariableName, domainInformation: DomainInformation): DefinedDomainType =
    DefinedDomainType(domainInformation, name.name)

  }

object DefinedDomainType {
  implicit val definedDomainTypeEnc:Encoder[DefinedDomainType] = (a: DefinedDomainType) => a.serialize
  implicit val definedDomainTypeDec:Decoder[DefinedDomainType] = (c:HCursor) => for {
    name <- c.downField("name").as[String]
    domain <- c.downField("domain").as[DomainInformation]
  } yield DefinedDomainType(domain = domain, typeName = name)

}

case class DefinedDomainType(domain:DomainInformation, typeName:String) extends VariableType(name = typeName) {


  override def serialize: Json = {
    Json.obj(
      "name" -> Json.fromString(typeName),
      "domain" -> domain.asJson
    )
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  /*override def access(value: OpenlawValue, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = {
    keys.toList match {
      case Nil =>
        Success(Some(value))
      case head :: tail =>
        val headName = VariableName(head)
        VariableType.convert[OpenlawMap[VariableName, OpenlawValue]](value).flatMap { values =>
          (for {
            result <- values.get(headName)
            keyType <- domain.typeDefinition.get(headName)
          } yield keyType.varType(executionResult).access(result, name, tail, executionResult)) match {
            case Some(result) => result
            case None if domain.names.contains(headName) =>
              Success(None)
            case None =>
              Failure(s"properties '${keys.mkString(".")}' could not be resolved for the structured type $typeName. available properties ${structure.names.map(name => s"'${name.name}'").mkString(",")}")
          }
        }
    }
  }*/

  override def getTypeClass: Class[OpenlawMap[VariableName, OpenlawValue]] = classOf[OpenlawMap[VariableName, OpenlawValue]]

  /*override def keysType(keys: Seq[String], expression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] =
    keys.toList match {
      case Nil =>
        Success(AbstractDomainType)
      case head::tail =>
        val name = VariableName(head)
        domain.variableType match {
          case Some(variableType) =>
            varDefinition.varType(executionResult).keysType(tail, expression, executionResult)
          case None =>
            Failure(s"property '${keys.mkString(".")}' could not be resolved in domain value '$head'")
        }
    }*/

  /*override def validateKeys(name:VariableName, keys: Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] = keys.toList match {
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
  }*/

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OpenlawMap[VariableName, OpenlawValue]] =
    for {
      values <- decode[Map[String, String]](value).leftMap(FailureException(_))
      list <- List(typeName, Map("variableName" -> domain.variableType, "validation" -> domain.validation)).sequence
    } yield OpenlawMap(list.toMap)

  /*override def internalFormat(value: OpenlawValue): Result[String] =
    VariableType.convert[OpenlawMap[VariableName, OpenlawValue]](value).flatMap { values =>
      domain
        .types
        .toList
        .sequence
        .map(_.toMap)
        .map { _.asJson.noSpaces }
    }*/

  override def thisType: VariableType = this
}
