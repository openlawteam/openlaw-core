package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder, HCursor, Json}
import cats.kernel.Eq
import org.adridadou.openlaw.parser.template._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.adridadou.openlaw._
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, ResultNel, Success}
import org.adridadou.openlaw.result.Implicits._

final case class DomainInformation(typeDefinition: VariableType, validation:Validation) extends OpenlawNativeValue {
	def validate(value:OpenlawValue, executionResult: TemplateExecutionResult):ResultNel[Unit] =
		executionResult.withVariable(VariableName("this"), value, typeDefinition).toResultNel
			.andThen(validation.validate)
}

case object DomainInformation {
  implicit val domainEnc:Encoder[DomainInformation] = deriveEncoder
  implicit val domainDec:Decoder[DomainInformation] = deriveDecoder
  implicit val domainDecEq:Eq[DomainInformation] = Eq.fromUniversalEquals

}

case object AbstractDomainType extends VariableType(name = "DomainType") with TypeGenerator[DomainInformation] {

	private def getValue(values:Map[String, Parameter], propertyName:String):Result[Parameter] = values
		.get(propertyName)
		.map(Success(_))
  	.getOrElse(Failure(s"missing property $propertyName in Domain Type constructor"))

	private def getVariableType(param:Parameter, executionResult: TemplateExecutionResult):Result[VariableType] = param match {
		case OneValueParameter(VariableName(typeName)) =>
			executionResult
				.findVariableType(VariableTypeDefinition(typeName)).map(Success(_))
				.getOrElse(Failure(s"type $typeName not found"))
		case OneValueParameter(definition:VariableDefinition) =>
			(for {
				varTypeDefinition <- definition.variableTypeDefinition
				varType <- executionResult.findVariableType(varTypeDefinition)
			} yield varType).map(Success(_)).getOrElse(Failure(s"type ${definition.name} not found"))
		case _ =>
			Failure("error in the constructor for Domain Type")
	}

  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Result[Option[DomainInformation]] = param match {
    case Parameters(values) =>
			val mappedValues = values.toMap
			for {
				varTypeParameter <- getValue(mappedValues, "variableType")
				variableType <- getVariableType(varTypeParameter, executionResult)
				validation <- ValidationType.construct2(param, executionResult)
			} yield Some(DomainInformation(variableType, validation))

    case _ =>
      Failure("""Domain type requires parameters (either 'variableType', 'condition', or 'errorMessage' is missing)""")
    }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[DomainInformation] = Failure("domain type definition cannot be casted")

  override def internalFormat(value: OpenlawValue): Result[String] = Failure("no internal format for domain type definition")

  override def getTypeClass: Class[_ <: DomainInformation] = classOf[DomainInformation]

  override def checkTypeName(nameToCheck: String): Boolean = Seq("DomainType").exists(_.equalsIgnoreCase(nameToCheck))

  def thisType: VariableType = AbstractDomainType

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

final case class DefinedDomainType(domain:DomainInformation, typeName:String) extends VariableType(name = typeName) {
  override def serialize: Json = {
    Json.obj(
      "name" -> Json.fromString(typeName),
      "domain" -> domain.asJson
    )
  }

	override def plus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
		domain.typeDefinition.plus(optLeft, optRight, executionResult)

	override def minus(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
		domain.typeDefinition.minus(optLeft, optRight, executionResult)

	override def multiply(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
		domain.typeDefinition.multiply(optLeft, optRight, executionResult)

	override def divide(optLeft: Option[OpenlawValue], optRight: Option[OpenlawValue], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
		domain.typeDefinition.divide(optLeft, optRight, executionResult)

	override def defaultFormatter: Formatter = new NoopFormatter

  override def access(value: OpenlawValue, name:VariableName, keys: Seq[String], executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] =
		domain.typeDefinition.access(value, name, keys, executionResult)

  override def getTypeClass: Class[_ <: OpenlawValue] = domain.typeDefinition.getTypeClass

  override def keysType(keys: Seq[String], expression: Expression, executionResult: TemplateExecutionResult): Result[VariableType] =
		domain.typeDefinition.keysType(keys, expression, executionResult)

  override def validateKeys(name:VariableName, keys: Seq[String], expression:Expression, executionResult: TemplateExecutionResult): Result[Unit] =
		domain.typeDefinition.validateKeys(name, keys, expression, executionResult)

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OpenlawValue] =
		domain.typeDefinition.cast(value, executionResult)

  override def internalFormat(value: OpenlawValue): Result[String] =
		domain.typeDefinition.internalFormat(value)

  override def thisType: VariableType = this
}
