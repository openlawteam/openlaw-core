package org.adridadou.openlaw.parser.template.variableTypes

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._
import cats.implicits._
import io.circe.generic.semiauto._
import org.adridadou.openlaw.{OpenlawNativeValue, OpenlawValue}
import org.adridadou.openlaw.parser.template.{OneValueParameter, OpenlawExecutionState, Parameter, TemplateExecutionResult, VariableDefinition, VariableName}
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.formatters.{Formatter, NoopFormatter}
import org.adridadou.openlaw.result.{Failure, Result, Success}
import org.adridadou.openlaw.result.Implicits._

object AbstractFunctionType extends VariableType(name = "Function") with ParameterTypeProvider {
  override def defaultFormatter: Formatter = new NoopFormatter

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OLFunction] = Failure("function type definition cannot be casted")

  override def internalFormat(value: OpenlawValue): Result[String] = Failure("no internal format for function type definition")

  override def getTypeClass: Class[_ <: OLFunction] = classOf[OLFunction]

  override def checkTypeName(nameToCheck: String): Boolean = Seq("Function").exists(_.equalsIgnoreCase(nameToCheck))

  def thisType: VariableType = AbstractStructureType

  override def createParameterInstance(typeParameter: VariableType): FunctionType =
    FunctionType(typeParameter)

}


final case class FunctionType(typeParameter:VariableType) extends VariableType(name = typeParameter.name) with ParameterType {
  override def serialize: Json =
    Json.obj(
      "name" -> typeParameter.name.asJson,
    )

  override def construct(param:Parameter, executionResult: TemplateExecutionResult): Result[Option[OLFunction]] = param match {
    case OneValueParameter(function:OLFunction) =>
      Success(Some(function))
    case parameter =>
      Failure(s"function must have one function as constructor parameter, instead received ${parameter.getClass}")
  }

  override def defaultFormatter: Formatter = new NoopFormatter

  override def getTypeClass: Class[OLFunction] = classOf[OLFunction]

  override def thisType: VariableType = this

  override def cast(value: String, executionResult: TemplateExecutionResult): Result[OpenlawValue] = Failure("function type cannot cast")

  override def internalFormat(value: OpenlawValue): Result[String] = Failure("function type cannot format")
}

object OLFunction {
  implicit val definedOLFunctionEnc:Encoder[OLFunction] = deriveEncoder
  implicit val definedOLFunctionDec:Decoder[OLFunction] = deriveDecoder
}

final case class OLFunctionCall(name:VariableName, parameter: Expression) extends Expression {
  override def missingInput(executionResult: TemplateExecutionResult): Result[List[VariableName]] = executionResult
    .getVariable(name)
    .map(_.evaluateT[OLFunction](executionResult)).sequence
    .map(_.flatten)
    .flatMap({
      case Some(func) =>
        for {
          ier <- innerExecutionResult(func, executionResult)
          missingVariables <- func.expression.missingInput(ier)
        } yield missingVariables
      case None =>
        Failure("function definition was not provided!")
    })

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] = executionResult.getVariable(name) match {
    case Some(variable) => variable.varType(executionResult) match {
      case _:FunctionType =>
        variable.construct(executionResult).flatMap({
          case Some(_:OLFunction) =>
            Success.unit
          case Some(value) =>
            Failure(s"constructor returns ${value.getClass.getSimpleName} instead of a function definition")
          case None =>
            Failure(s"constructor returns nothing!")
        })
      case otherType => Failure(s"${name.name} should be a function but instead is ${otherType.name}")
    }
    case None =>
      Failure(s"function ${name.name} could not be found")
  }

  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] = for {
    variable <- executionResult.getVariable(name).toResult(s"function definition ${name.name} cannot be found")
    func <- variable.evaluateT[OLFunction](executionResult).flatMap(_.toResult(s"function definition missing for variable ${name.name}"))
    ier <- innerExecutionResult(func, executionResult)
    eType <- func.expression.expressionType(ier)
  } yield eType

  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = for {
    variable <- executionResult.getVariable(name).toResult(s"function definition ${name.name} cannot be found")
    func <- variable.evaluateT[OLFunction](executionResult).flatMap(_.toResult(s"function definition missing for variable ${name.name}"))
    ier <- innerExecutionResult(func, executionResult)
    value <- func.expression.evaluate(ier)
  } yield value

  override def variables(executionResult: TemplateExecutionResult): Result[List[VariableName]] = for {
    variable <- executionResult.getVariable(name).toResult(s"function definition ${name.name} cannot be found")
    func <- variable.evaluateT[OLFunction](executionResult).flatMap(_.toResult(s"function definition missing for variable ${name.name}"))
    variables <- func.expression.variables(executionResult)
  } yield variables.filter(_ =!= func.parameter.name)

  private def innerExecutionResult(func:OLFunction, executionResult: TemplateExecutionResult):Result[OpenlawExecutionState] = for {
    value <- parameter.evaluate(executionResult)
    ier <- executionResult.withVariable(func.parameter.name, value, func.parameter.varType(executionResult))
  } yield ier

}

final case class OLFunction(parameter:VariableDefinition, expression:Expression) extends OpenlawNativeValue with Expression {
  override def missingInput(executionResult: TemplateExecutionResult): Result[List[VariableName]] = for {
    ier <- innerExecutionResult(executionResult)
    r <- expression.missingInput(ier)
  } yield r

  override def validate(executionResult: TemplateExecutionResult): Result[Unit] = for {
    ier <- innerExecutionResult(executionResult)
    r <- expression.validate(ier)
  } yield r

  override def expressionType(executionResult: TemplateExecutionResult): Result[VariableType] = for {
    ier <- innerExecutionResult(executionResult)
    r <- expression.expressionType(ier)
  } yield r

  override def evaluate(executionResult: TemplateExecutionResult): Result[Option[OpenlawValue]] = for {
    ier <- innerExecutionResult(executionResult)
    r <- expression.evaluate(ier)
  } yield r

  override def variables(executionResult: TemplateExecutionResult): Result[List[VariableName]] = for {
    ier <- innerExecutionResult(executionResult)
    r <- expression.variables(ier)
  } yield r.filter(_ =!= parameter.name)

  private def innerExecutionResult(executionResult: TemplateExecutionResult) =
    executionResult.withVariable(parameter.name, None, parameter.varType(executionResult))
}