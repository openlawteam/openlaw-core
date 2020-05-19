package org.adridadou.openlaw.parser.template

import cats.implicits._
import cats.Eq
import java.util.concurrent.atomic.AtomicInteger

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.adridadou.openlaw.values.{TemplateParameters, TemplateTitle}
import org.adridadou.openlaw.parser.template.expressions.Expression
import org.adridadou.openlaw.parser.template.variableTypes._
import org.adridadou.openlaw.{
  createConcurrentMutableBuffer,
  createConcurrentMutableMap,
  createConcurrentMutableSet,
  OpenlawMap,
  OpenlawNativeValue,
  OpenlawValue
}
import org.adridadou.openlaw.oracles.SignatureProof
import org.adridadou.openlaw.result.{
  Failure,
  FailureCause,
  Result,
  ResultNel,
  Success
}
import org.adridadou.openlaw.vm.Executions

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import VariableName._
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.adridadou.openlaw.parser.template.formatters.Formatter

trait TemplateExecutionResult {

  private val expressionParser = new ExpressionParserService
  def id: TemplateExecutionResultId
  def templateDefinition: Option[TemplateDefinition]
  def subExecutions: Map[VariableName, TemplateExecutionResult]
  def signatureProofs: Map[Email, SignatureProof]
  def parentExecution: Option[TemplateExecutionResult]
  def variables: List[VariableDefinition]
  def mapping: Map[VariableName, Expression]
  def aliases: List[VariableAliasing]
  def sectionNameMappingInverse: Map[VariableName, String]
  def containSectionName(variableName: VariableName): Boolean
  def variableSections: Map[String, List[VariableName]]
  def parameters: TemplateParameters
  def executionType: ExecutionType
  def processedSections: List[(Section, Int)]
  def executedVariables: List[VariableName]
  def agreements: List[StructuredAgreement]
  def variableSectionList: List[String]
  def executions: Map[ActionIdentifier, Executions]
  def info: OLInformation
  def externalCallStructures: Map[ServiceName, IntegratedServiceDefinition]
  def toSerializable: SerializableTemplateExecutionResult

  def evaluate[T](
      variableName: VariableName
  )(implicit classTag: ClassTag[T]): Result[T] =
    evaluate(variableName.name)

  def evaluate[T](expr: String)(implicit classTag: ClassTag[T]): Result[T] =
    parseExpression(expr).flatMap(evaluate[T])

  def evaluate[T](expr: Expression)(implicit classTag: ClassTag[T]): Result[T] =
    expr.evaluate(this).flatMap {
      case Some(value) =>
        convert[T](value)
      case None =>
        Failure(s"could not resolve ${expr.toString}")
    }

  @tailrec
  private def convert[T](
      value: Any
  )(implicit classTag: ClassTag[T]): Result[T] = value match {
    case v: T => Success(v)
    case v: OpenlawNativeValue =>
      Failure(
        s"conversion error. Was expecting ${classTag.runtimeClass.getName} but got ${value.getClass.getName}"
      )
    case v: OpenlawValue => convert[T](v.underlying)
    case _ =>
      Failure(
        s"conversion error. Was expecting ${classTag.runtimeClass.getName} but got ${value.getClass.getName}"
      )
  }

  def parseExpression(expr: String): Result[Expression] =
    expressionParser.parseExpression(expr)

  def hasSigned(email: Email): Boolean =
    if (signatureProofs.contains(email)) true
    else parentExecution.exists(_.hasSigned(email))

  def findExecutionResult(
      executionResultId: TemplateExecutionResultId
  ): Option[TemplateExecutionResult] =
    if (id === executionResultId) {
      Some(this)
    } else {
      subExecutions.values
        .flatMap(_.findExecutionResult(executionResultId))
        .headOption
    }

  def getVariable(variable: VariableDefinition): Option[VariableDefinition] =
    getVariable(variable.name)

  def findVariable(name: VariableName): Option[VariableDefinition]
  def findVariable(
      name: VariableName,
      varType: VariableType
  ): Option[VariableDefinition]

  def getVariable(name: VariableName): Option[VariableDefinition] =
    if (this.containSectionName(name)) {
      this.findVariable(name, SectionType)
    } else {
      (mapping.get(name) match {
        case Some(_) =>
          None
        case None =>
          this.findVariable(name)
      }) match {
        case Some(variable) =>
          Some(variable)
        case None =>
          parentExecution
            .flatMap(_.getVariable(name))
      }
    }

  def getVariable(name: String): Option[VariableDefinition] =
    getVariable(VariableName(name))

  def getAlias(name: VariableName): Option[Expression] =
    (mapping.get(name) match {
      case Some(expression) =>
        parentExecution.map(MappingExpression(expression, _))
      case None =>
        aliases.find(_.name === name)
    }) match {
      case Some(alias) =>
        Some(alias)
      case None =>
        parentExecution
          .flatMap(_.getAlias(name))
    }

  def getAlias(name: String): Option[Expression] =
    getAlias(VariableName(name))

  def getAliasOrVariableType(name: VariableName): Result[VariableType] =
    getExpression(name)
      .map(_.expressionType(this))
      .getOrElse(Failure(s"${name.name} cannot be resolved!"))

  def getVariables: List[VariableDefinition] =
    variables
      .filter(variable => !mapping.contains(variable.name))
      .foldLeft(DistinctVariableBuilder())((builder, variable) =>
        if (builder.names.contains(variable.name)) {
          builder
        } else {
          builder.add(variable)
        }
      )
      .variables
      .toList

  def getVariables(
      varTypes: VariableType*
  ): List[(TemplateExecutionResult, VariableDefinition)] =
    getVariables
      .filter(variable =>
        variable.varType(this) match {
          case collectionType: CollectionType =>
            varTypes.contains(collectionType.typeParameter)
          case structuredType: DefinedStructureType =>
            structuredType.structure.typeDefinition.values
              .exists(s => varTypes.contains(s.varType(this)))
          case variableType => varTypes.contains(variableType)
        }
      )
      .map((this, _)) ++ subExecutions.values.flatMap(
      _.getVariables(varTypes: _*)
    )

  def getVariableValues[U <: OpenlawValue](
      varType: VariableType
  )(implicit classTag: ClassTag[U]): Result[List[U#T]] =
    getVariables(varType)
      .map {
        case (execution, variable) =>
          variable
            .evaluate(execution)
            .flatMap(
              _.map(getVariableValue[U](_, variable.varType(this))).sequence
            )
      }
      .sequence
      .map(_.flatten)

  def getVariableValue[U <: OpenlawValue](
      name: VariableName
  )(implicit classTag: ClassTag[U]): Result[Option[U#T]] =
    getVariable(name)
      .map(variable =>
        variable
          .evaluate(this)
          .flatMap(
            _.map(getVariableValue[U](_, variable.varType(this))).sequence
          )
      )
      .sequence
      .map(_.flatten)

  def getVariableValue[U <: OpenlawValue](
      value: OpenlawValue,
      variableType: VariableType
  )(implicit classTag: ClassTag[U]): Result[U#T] =
    VariableType.convert[U](value)

  def getParameter(name: String): Option[String] =
    getParameter(VariableName(name))

  def getParameter(name: VariableName): Option[String] =
    parameters.get(name) match {
      case Some(value) =>
        Some(value)
      case None =>
        parentExecution.flatMap(_.getParameter(name))
    }

  def allIdentityEmails: Result[List[Email]] =
    allIdentities().map(_.map(_.email).distinct)

  def allIdentities(): Result[List[Identity]] = {
    getAllExecutedVariables
      .flatMap {
        case (result, name) =>
          result.getVariable(name).map(variable => (result, variable))
      }
      .map {
        case (result, variable) =>
          variable.varType(result) match {
            case IdentityType =>
              variable.evaluateT[Identity](result).map(_.toSeq)
            case ExternalSignatureType =>
              variable
                .evaluateT[ExternalSignature](result)
                .map(_.flatMap(_.identity).toSeq)

            case collectionType: CollectionType
                if collectionType.typeParameter === IdentityType =>
              variable
                .evaluateT[CollectionValue](result)
                .flatMap {
                  _.map(x => x.list.map(VariableType.convert[Identity]).sequence
                  ).getOrElse(Success(Seq.empty))
                }

            case collectionType: CollectionType
                if collectionType.typeParameter === ExternalSignatureType =>
              variable
                .evaluateT[CollectionValue](result)
                .flatMap {
                  _.map(x =>
                    x.list.map(VariableType.convert[ExternalSignature]).sequence
                  ).getOrElse(Success(Seq.empty))
                }
                .map(_.flatMap(_.identity))

            case structureType: DefinedStructureType
                if structureType.structure.typeDefinition.values.exists(
                  _.varType(result) === IdentityType
                ) =>
              variable
                .evaluateT[OpenlawMap[VariableName, OpenlawValue]](result)
                .map(_.getOrElse(Map.empty))
                .flatMap { values =>
                  structureType.structure.names
                    .filter(name =>
                      structureType.structure
                        .typeDefinition(name)
                        .varType(result) === IdentityType
                    )
                    .map(name => VariableType.convert[Identity](values(name)))
                    .sequence
                }

            case structureType: DefinedStructureType
                if structureType.structure.typeDefinition.values.exists(
                  _.varType(result) === ExternalSignatureType
                ) =>
              variable
                .evaluateT[OpenlawMap[VariableName, OpenlawValue]](result)
                .map(_.getOrElse(Map.empty))
                .flatMap { values =>
                  structureType.structure.names
                    .filter(name =>
                      structureType.structure
                        .typeDefinition(name)
                        .varType(result) === ExternalSignatureType
                    )
                    .map(name =>
                      VariableType.convert[ExternalSignature](values(name))
                    )
                    .sequence
                    .map(_.flatMap(_.identity))
                }
            case _ =>
              Success(Nil)
          }
      }
      .sequence
      .map(_.flatten)
  }

  def allActions: Result[List[ActionInfo]] =
    getAllExecutedVariables
      .map {
        case (executionResult, variableName) =>
          executionResult
            .getVariable(variableName)
            .map { variable =>
              (variable.varType(executionResult) match {
                case actionType: ActionType =>
                  variable
                    .evaluate(executionResult)
                    .flatMap(_.map(actionType.actionValue).sequence)
                case _ =>
                  Success(None)
              }).map(x =>
                x.map(action => ActionInfo(action, executionResult)).toList
              )
            }
            .toList
            .sequence
            .map(_.flatten)
      }
      .sequence
      .map(_.flatten)

  def getTemplateDefinitionForVariable(
      name: VariableName
  ): Option[TemplateDefinition] = findVariable(name) match {
    case Some(_) =>
      templateDefinition
    case None =>
      parentExecution.flatMap(_.getTemplateDefinitionForVariable(name))
  }

  def embedded: Boolean = executionType match {
    case TemplateExecution => false
    case _                 => true
  }

  def allProcessedSections: List[(Section, Int)] =
    (embedded, parentExecution) match {
      case (true, Some(parent)) =>
        parent.allProcessedSections ++ processedSections
      case _ => processedSections
    }

  def allAliases: List[(TemplateExecutionResult, VariableAliasing)] =
    aliases.map(alias => (this, alias)) ++ subExecutions.values.flatMap(
      _.allAliases
    )

  def getAllExecutedVariables: List[(TemplateExecutionResult, VariableName)] =
    executedVariables.distinct.map(name => (this, name)) ++ subExecutions.values
      .flatMap(_.getAllExecutedVariables)

  @tailrec
  final def getAllVariables
      : List[(TemplateExecutionResult, VariableDefinition)] =
    parentExecution match {
      case Some(execution) =>
        execution.getAllVariables
      case None =>
        getAllVariablesFromRoot
    }

  private def getAllVariablesFromRoot
      : List[(TemplateExecutionResult, VariableDefinition)] =
    getVariables.map(variable => (this, variable)) ++ subExecutions.values
      .flatMap(_.getAllVariablesFromRoot)
      .toList

  def getVariableNames: List[VariableName] =
    getVariables
      .foldLeft(DistinctVariableBuilder())({
        case (builder, variable) =>
          if (builder.names.contains(variable.name)) {
            builder
          } else {
            builder.add(variable)
          }
      })
      .variables
      .map(_.name)
      .toList

  def getAllVariableNames: List[VariableName] =
    getAllVariables
      .foldLeft(DistinctVariableBuilder())({
        case (builder, (_, variable)) =>
          if (builder.names.contains(variable.name)) {
            builder
          } else {
            builder.add(variable)
          }
      })
      .variables
      .map(_.name)
      .toList

  def findVariableType(
      variableTypeDefinition: VariableTypeDefinition
  ): Option[VariableType] = {
    val mainType = findVariableTypeAllDirection(variableTypeDefinition)
    val parameterType =
      variableTypeDefinition.typeParameter.flatMap(findVariableTypeAllDirection)
    mainType match {
      case Some(varType: ParameterTypeProvider) =>
        Some(varType.createParameterInstance(parameterType.getOrElse(TextType)))
      case other =>
        other
    }
  }

  private def findVariableTypeAllDirection(
      variableTypeDefinition: VariableTypeDefinition
  ): Option[VariableType] =
    findVariableTypeInternalCurrent(variableTypeDefinition) orElse
      findVariableTypeInternalParent(variableTypeDefinition) orElse
      findVariableTypeInternalEmbedded(variableTypeDefinition)
  def findVariableTypeInternalCurrent(
      variableTypeDefinition: VariableTypeDefinition
  ): Option[VariableType]

  def findVariableTypeInternalParent(
      variableTypeDefinition: VariableTypeDefinition
  ): Option[VariableType] =
    parentExecution.flatMap(parent =>
      parent
        .findVariableTypeInternalCurrent(variableTypeDefinition) orElse parent
        .findVariableTypeInternalParent(variableTypeDefinition)
    )

  def findVariableTypeInternalEmbedded(
      variableTypeDefinition: VariableTypeDefinition
  ): Option[VariableType] =
    subExecutions.values
      .filter(_.embedded)
      .flatMap(subExecution =>
        subExecution
          .findVariableTypeInternalCurrent(variableTypeDefinition) orElse subExecution
          .findVariableType(variableTypeDefinition)
      )
      .headOption

  def getSignatureProof(identity: Identity): Option[SignatureProof] =
    signatureProofs.get(identity.email) match {
      case Some(value) => Some(value)
      case None        => parentExecution.flatMap(_.getSignatureProof(identity))
    }

  def sections: Map[String, Seq[VariableName]] =
    variableSections
      .map({
        case (name, sectionVariables) => name -> sectionVariables.distinct
      })

  def getExpression(name: VariableName): Option[Expression] =
    getAlias(name) match {
      case Some(alias) =>
        Some(alias)
      case None =>
        getVariable(name)
    }

  private def validateGlobalValidation: ResultNel[Unit] = {
    val result = getAllExecutedVariables
      .map({
        case (executionResult, name) =>
          for {
            validationResult <- executionResult
              .getVariable(name)
              .filter(_.varType(executionResult) === ValidationType)
              .map(_.evaluateT[Validation](executionResult))
              .sequence
              .map(_.flatten)
          } yield validationResult
            .map(_.validate(executionResult))
            .getOrElse(Valid(()))

      })
      .sequence

    result
      .map(_.reduceOption(_ combine _).getOrElse(Valid(()))) match {
      case f: FailureCause           => Invalid(NonEmptyList(f, Nil))
      case Success(validationResult) => validationResult
    }
  }

  private def validateDomainTypeVariables: ResultNel[Unit] = {
    val result = getAllExecutedVariables
      .map({
        case (executionResult, name) =>
          for {
            domainTypeValue <- executionResult
              .getVariable(name)
              .map(variable =>
                variable.varType(executionResult) match {
                  case _: DefinedDomainType =>
                    variable.evaluate(executionResult)
                  case _ => Success(None)
                }
              )
              .sequence
              .map(_.flatten)
          } yield {
            val domainTypeResult = executionResult
              .getVariable(name)
              .flatMap(_.varType(executionResult) match {
                case varType: DefinedDomainType => Some(varType)
                case _                          => None
              })
            (for {
              dtv <- domainTypeValue
              dtr <- domainTypeResult
            } yield dtr.domain.validate(dtv, executionResult))
              .getOrElse(Valid(()))
          }
      })
      .sequence

    result
      .map(_.reduceOption(_ combine _).getOrElse(Valid(()))) match {
      case f: FailureCause           => Invalid(NonEmptyList(f, Nil))
      case Success(validationResult) => validationResult
    }
  }

  private def validateDomainTypeExpressions: ResultNel[Unit] = {
    val result = allAliases
      .map({
        case (executionResult, alias) =>
          for {
            domainTypeValue <- alias
              .expressionType(executionResult)
              .flatMap({
                case _: DefinedDomainType => alias.evaluate(executionResult)
                case _                    => Success(None)
              })
            domainTypeResult <- alias
              .expressionType(executionResult)
              .map({
                case varType: DefinedDomainType => Some(varType)
                case _                          => None
              })
          } yield {
            (for {
              dtv <- domainTypeValue
              dtr <- domainTypeResult
            } yield dtr.domain.validate(dtv, executionResult))
              .getOrElse(Valid(()))
          }
      })
      .sequence

    result
      .map(_.reduceOption(_ combine _).getOrElse(Valid(()))) match {
      case f: FailureCause           => Invalid(NonEmptyList(f, Nil))
      case Success(validationResult) => validationResult
    }
  }

  def validate: ResultNel[Unit] =
    validateGlobalValidation combine validateDomainTypeVariables combine validateDomainTypeExpressions

  def getExecutedVariables: List[VariableName] = {
    val variableNames = getAllVariableNames.toSet
    getAllExecutedVariables
      .filter({ case (_, name) => variableNames.contains(name) })
      .map({ case (_, name) => name })
      .distinct
  }

  def getAllExecutionResults: List[TemplateExecutionResult] =
    this :: subExecutions.values.flatMap(_.getAllExecutionResults).toList

  def toExecutionState: OpenlawExecutionState

  def withVariable(
      name: VariableName,
      value: OpenlawValue,
      varType: VariableType
  ): Result[OpenlawExecutionState] =
    withVariable(name, Some(value), varType)

  def withVariable(
      name: VariableName,
      value: Option[OpenlawValue],
      varType: VariableType
  ): Result[OpenlawExecutionState] =
    this.getAliasOrVariableType(name) match {
      case Success(_) =>
        Failure(s"${name.name} has already been defined!")
      case Failure(_, _) =>
        for {
          parameters <- value
            .map(varType.internalFormat)
            .map(
              _.map(internalFormat =>
                this.parameters ++ Map(name -> internalFormat)
              )
            )
            .getOrElse(Success(this.parameters))
        } yield {
          val executionState = this.toExecutionState
          val newResult = executionState.copy(
            parameters = parameters,
            variablesInternal =
              createConcurrentMutableBuffer(executionState.variablesInternal),
            executedVariablesInternal = createConcurrentMutableBuffer(
              executionState.executedVariablesInternal
            )
          )

          val r = newResult.registerNewType(varType) match {
            case Success(newResult) => newResult
            case Failure(_, _)      => newResult
          }

          r.variablesInternal.append(
            VariableDefinition(
              name,
              Some(VariableTypeDefinition(name = varType.name, None))
            )
          )
          r.executedVariablesInternal.append(name)
          r
        }
    }

  def validateExecution: Result[ValidationResult] = {
    val variables = getAllExecutedVariables
      .flatMap({
        case (result, name) =>
          result.getVariable(name).map(variable => (result, variable))
      })
      .filter({
        case (result, variable) =>
          variable.varType(result) match {
            case _: NoShowInForm => false
            case _               => true
          }
      })

    val identities = variables.filter({
      case (result, variable) =>
        variable.varType(result) match {
          case IdentityType          => true
          case ExternalSignatureType => true
          case collectionType: CollectionType
              if IdentityType.identityTypes
                .contains(collectionType.typeParameter) =>
            true
          case structureType: DefinedStructureType
              if structureType.structure.typeDefinition.values.exists(s =>
                IdentityType.identityTypes.contains(s.varType(this))
              ) =>
            true
          case domainType: DefinedDomainType
              if IdentityType.identityTypes
                .contains(domainType.domain.typeDefinition) =>
            true
          case _ =>
            false
        }
    })

    val missingIdentitiesResult = identities.map {
      case (result, variable) =>
        variable.varType(result) match {
          case IdentityType =>
            Success(resultFromMissingInput(variable.missingInput(result)))
          case ExternalSignatureType =>
            Success(resultFromMissingInput(variable.missingInput(result)))

          case _: CollectionType =>
            result.getVariableValue[CollectionValue](variable.name).map {
              case Some(value) if value.size =!= value.values.size =>
                (List(variable.name), Nil)
              case Some(_) =>
                (Nil, Nil)
              case None =>
                (List(variable.name), Nil)
            }

          case structureType: DefinedStructureType =>
            result
              .getVariableValue[OpenlawMap[VariableName, OpenlawValue]](
                variable.name
              )
              .map { values =>
                val identityProperties =
                  structureType.structure.typeDefinition
                    .filter({
                      case (_, propertyType) =>
                        IdentityType.identityTypes
                          .contains(propertyType.varType(this))
                    })
                    .map({ case (propertyName, _) => propertyName })

                if (identityProperties.forall(
                      values.getOrElse(Map.empty).contains
                    )) {
                  (Nil, Nil)
                } else {
                  (List(variable.name), Nil)
                }
              }
          case _ =>
            Success((Nil, Nil))
        }
    }.sequence

    missingIdentitiesResult.map { missingIdentitiesValue =>
      val identitiesErrors = missingIdentitiesValue.flatMap({
        case (_, errors) => errors
      })

      val missingIdentities = missingIdentitiesValue.flatMap({
        case (values, _) => values
      })

      val (missingInputs, additionalErrors) =
        resultFromMissingInput(allMissingInput)

      val validationErrors =
        validate.leftMap(nel => nel.map(_.message).toList).swap.getOrElse(Nil)

      ValidationResult(
        identities = identities.map({ case (_, variable) => variable }),
        missingInputs = missingInputs,
        missingIdentities = missingIdentities,
        validationExpressionErrors = validationErrors ++ additionalErrors ++ identitiesErrors
      )
    }
  }

  def allMissingInput: Result[List[VariableName]] = {
    val missingInputs = getAllExecutedVariables
      .map({ case (result, variable) => variable.missingInput(result) })

    missingInputs.sequence.map(_.flatten.distinct)
  }

  def resultFromMissingInput(
      lst: Result[List[VariableName]]
  ): (List[VariableName], List[String]) = lst match {
    case Success(inputs)     => (inputs, Nil)
    case Failure(_, message) => (Nil, List(message))
  }
}

object SerializableTemplateExecutionResult {
  implicit val serializableTemplateExecutionResultEnc
      : Encoder[SerializableTemplateExecutionResult] =
    (a: SerializableTemplateExecutionResult) =>
      Json.obj(
        "templateExecutions" -> a.templateExecutions
          .map({
            case (id, executionResult) =>
              id.id -> encodeNoDependencies(executionResult)
          })
          .asJson,
        "executionResult" -> encodeNoDependencies(a)
      )

  private val derivedEncoder =
    deriveEncoder[SerializableTemplateExecutionResult]
  private val derivedDecoder =
    deriveDecoder[SerializableTemplateExecutionResult]

  private def encodeNoDependencies(
      executionResult: SerializableTemplateExecutionResult
  ): Json =
    derivedEncoder(executionResult.copy(templateExecutions = Map.empty))

  implicit val serializableTemplateExecutionResultDec
      : Decoder[SerializableTemplateExecutionResult] = (c: HCursor) =>
    for {
      mainExecutionResultJson <- c.downField("executionResult").as[Json]
      mainExecutionResult <- derivedDecoder(mainExecutionResultJson.hcursor)
      templateExecutionsJson <- c
        .downField("templateExecutions")
        .as[Map[TemplateExecutionResultId, Json]]
      templateExecutions <- templateExecutionsJson
        .map({ case (id, json) => derivedDecoder(json.hcursor).map(id -> _) })
        .toList
        .sequence
    } yield {
      val templateExecutionsMap = templateExecutions.toMap

      val allTemplateExecutions = templateExecutionsMap + (mainExecutionResult.id -> mainExecutionResult)

      val allTemplateExecutions2 = allTemplateExecutions.map({
        case (id, e) => id -> e.copy(templateExecutions = allTemplateExecutions)
      })

      mainExecutionResult.copy(templateExecutions = allTemplateExecutions.map({
        case (id, executionResult) =>
          id -> executionResult.copy(templateExecutions = allTemplateExecutions2
          )
      }))
    }

  implicit val serializableTemplateExecutionResultEq
      : Eq[SerializableTemplateExecutionResult] = Eq.fromUniversalEquals
}

final case class SerializableTemplateExecutionResult(
    id: TemplateExecutionResultId,
    info: OLInformation,
    templateDefinition: Option[TemplateDefinition] = None,
    subExecutionIds: Map[VariableName, TemplateExecutionResultId],
    templateExecutions: Map[
      TemplateExecutionResultId,
      SerializableTemplateExecutionResult
    ],
    executions: Map[ActionIdentifier, Executions],
    parentExecutionId: Option[TemplateExecutionResultId],
    agreements: List[StructuredAgreement],
    variableSectionList: List[String],
    signatureProofs: Map[Email, SignatureProof],
    variables: List[VariableDefinition],
    executedVariables: List[VariableName],
    mapping: Map[VariableName, Expression],
    aliases: List[VariableAliasing],
    sectionNameMappingInverse: Map[VariableName, String],
    variableTypes: Map[String, VariableType],
    variableSections: Map[String, List[VariableName]],
    parameters: TemplateParameters,
    executionType: ExecutionType,
    processedSections: List[(Section, Int)],
    externalCallStructures: Map[ServiceName, IntegratedServiceDefinition]
) extends TemplateExecutionResult {

  override def toSerializable: SerializableTemplateExecutionResult = this
  override def findVariable(
      name: VariableName
  ): Option[
    VariableDefinition
  ] = variables.find(_.name === name)

  override def findVariable(
      name: VariableName,
      varType: VariableType
  ): Option[
    VariableDefinition
  ] =
    variables.find(v =>
      v.name === name && v.variableTypeDefinition.exists(
        _.name === varType.name
      )
    )

  override def findVariableTypeInternalCurrent(
      variableTypeDefinition: VariableTypeDefinition
  ): Option[VariableType] = variableTypes.get(variableTypeDefinition.name)

  override def containSectionName(
      variableName: VariableName
  ): Boolean = sectionNameMappingInverse.contains(variableName)

  override def toExecutionState: OpenlawExecutionState = OpenlawExecutionState(
    id = this.id,
    parameters = this.parameters,
    executionType = this.executionType,
    info = this.info,
    executions = this.executions,
    signatureProofs = this.signatureProofs,
    template = CompiledAgreement(),
    variablesInternal = createConcurrentMutableBuffer(this.variables),
    aliasesInternal = createConcurrentMutableBuffer(this.aliases),
    executedVariablesInternal =
      createConcurrentMutableBuffer(this.executedVariables),
    variableSectionsInternal = createConcurrentMutableMap(
      this.variableSections
        .map({
          case (name, sections) =>
            name -> createConcurrentMutableBuffer(sections)
        })
    ),
    variableSectionListInternal =
      createConcurrentMutableBuffer(this.variableSectionList),
    agreementsInternal = createConcurrentMutableBuffer(this.agreements),
    subExecutionsInternal = createConcurrentMutableMap(this.subExecutions.map({
      case (name, subExecution) => name -> subExecution.toExecutionState
    })),
    state = ExecutionFinished,
    parentExecution = this.parentExecution,
    compiledAgreement = None,
    variableRedefinition = VariableRedefinition(),
    templateDefinition = this.templateDefinition,
    mapping = this.mapping,
    variableTypesInternal = createConcurrentMutableMap(this.variableTypes),
    externalCallStructures = this.externalCallStructures
  )

  override def subExecutions: Map[VariableName, TemplateExecutionResult] =
    subExecutionIds.flatMap({
      case (identifier, executionId) =>
        templateExecutions.get(executionId).map(identifier -> _)
    })
  override def parentExecution: Option[TemplateExecutionResult] =
    parentExecutionId.flatMap(templateExecutions.get)
}

object ExecutionType {
  implicit val executionTypeEq: Eq[ExecutionType] = Eq.fromUniversalEquals
}

sealed trait ExecutionType
case object TemplateExecution extends ExecutionType
case object ClauseExecution extends ExecutionType
case object BlockExecution extends ExecutionType

object OpenlawExecutionState {
  val empty: OpenlawExecutionState = OpenlawExecutionState(
    id = TemplateExecutionResultId("@@anonymous_main_template_id@@"),
    info = OLInformation(),
    template = CompiledAgreement(),
    executions = Map.empty,
    executionType = TemplateExecution,
    remainingElements = createConcurrentMutableBuffer,
    signatureProofs = Map.empty,
    parameters = TemplateParameters(),
    variableRedefinition = VariableRedefinition()
  )
}

final case class OpenlawExecutionState(
    id: TemplateExecutionResultId,
    parameters: TemplateParameters,
    executionType: ExecutionType,
    info: OLInformation,
    executions: Map[ActionIdentifier, Executions],
    signatureProofs: Map[Email, SignatureProof] = Map.empty,
    template: CompiledTemplate,
    anonymousVariableCounter: AtomicInteger = new AtomicInteger(0),
    processedAnonymousVariableCounter: AtomicInteger = new AtomicInteger(0),
    variablesInternal: mutable.Buffer[VariableDefinition] =
      createConcurrentMutableBuffer,
    aliasesInternal: mutable.Buffer[VariableAliasing] =
      createConcurrentMutableBuffer,
    executedVariablesInternal: mutable.Buffer[VariableName] =
      createConcurrentMutableBuffer,
    variableSectionsInternal: mutable.Map[String, mutable.Buffer[
      VariableName
    ]] = createConcurrentMutableMap,
    variableSectionListInternal: mutable.Buffer[String] =
      createConcurrentMutableBuffer,
    agreementsInternal: mutable.Buffer[StructuredAgreement] =
      createConcurrentMutableBuffer,
    subExecutionsInternal: mutable.Map[VariableName, OpenlawExecutionState] =
      createConcurrentMutableMap,
    forEachExecutions: mutable.Buffer[TemplateExecutionResultId] =
      createConcurrentMutableBuffer,
    finishedEmbeddedExecutions: mutable.Buffer[OpenlawExecutionState] =
      createConcurrentMutableBuffer,
    state: TemplateExecutionState = ExecutionReady,
    remainingElements: mutable.Buffer[TemplatePart] =
      createConcurrentMutableBuffer,
    parentExecution: Option[TemplateExecutionResult] = None,
    compiledAgreement: Option[CompiledAgreement] = None,
    variableRedefinition: VariableRedefinition,
    templateDefinition: Option[TemplateDefinition] = None,
    mapping: Map[VariableName, Expression] = Map.empty,
    variableTypesInternal: mutable.Map[String, VariableType] =
      createConcurrentMutableMap(VariableType.allTypesMap),
    sectionLevelStack: mutable.Buffer[Int] = createConcurrentMutableBuffer,
    sectionNameMapping: mutable.Map[String, VariableName] =
      createConcurrentMutableMap,
    sectionNameMappingInverseInternal: mutable.Map[VariableName, String] =
      createConcurrentMutableMap,
    processedSectionsInternal: mutable.Buffer[(Section, Int)] =
      createConcurrentMutableBuffer,
    lastSectionByLevel: mutable.Map[Int, String] = createConcurrentMutableMap,
    externalCallStructures: Map[ServiceName, IntegratedServiceDefinition] =
      Map.empty
) extends TemplateExecutionResult {

  override def findVariable(
      name: VariableName
  ): Option[
    VariableDefinition
  ] = variablesInternal.find(_.name === name)

  override def findVariable(
      name: VariableName,
      varType: VariableType
  ): Option[
    VariableDefinition
  ] =
    variablesInternal.find(v =>
      v.name === name && v.variableTypeDefinition.exists(
        _.name === varType.name
      )
    )

  def variables: List[VariableDefinition] = variablesInternal.toList
  def aliases: List[VariableAliasing] = aliasesInternal.toList
  def processedSections: List[(Section, Int)] = processedSectionsInternal.toList
  def executedVariables: List[VariableName] = executedVariablesInternal.toList
  def agreements: List[StructuredAgreement] = agreementsInternal.toList
  def variableSectionList: List[String] = variableSectionListInternal.toList

  override def findVariableTypeInternalCurrent(
      variableTypeDefinition: VariableTypeDefinition
  ): Option[
    VariableType
  ] = variableTypesInternal.get(variableTypeDefinition.name)

  override def toExecutionState: OpenlawExecutionState = this

  override def variableSections: Map[String, List[VariableName]] =
    variableSectionsInternal
      .map({ case (key, value) => key -> value.toList })
      .toMap

  override def sectionNameMappingInverse: Map[VariableName, String] =
    sectionNameMappingInverseInternal.toMap

  override def containSectionName(
      variableName: VariableName
  ): Boolean = sectionNameMappingInverseInternal.contains(variableName)

  @tailrec
  def addLastSectionByLevel(lvl: Int, sectionValue: String): Unit = {
    if (embedded) {
      parentExecution match {
        case Some(parent: OpenlawExecutionState) =>
          parent.addLastSectionByLevel(lvl, sectionValue)
        case None =>
          lastSectionByLevel put (lvl, sectionValue)
      }
    } else {
      lastSectionByLevel put (lvl, sectionValue)
    }
  }

  override def subExecutions: Map[VariableName, TemplateExecutionResult] =
    subExecutionsInternal.toMap

  @scala.annotation.tailrec
  def getLastSectionByLevel(idx: Int): String = {
    if (embedded) {
      parentExecution match {
        case Some(parent: OpenlawExecutionState) =>
          parent.getLastSectionByLevel(idx)
        case None => lastSectionByLevel.getOrElse(idx, "")
      }
    } else {
      lastSectionByLevel.getOrElse(idx, "")
    }
  }

  @scala.annotation.tailrec
  def addProcessedSection(section: Section, number: Int): Unit =
    (embedded, parentExecution) match {
      case (true, Some(parent: OpenlawExecutionState)) =>
        parent.addProcessedSection(section, number)
      case _ => processedSectionsInternal append (section -> number)
    }

  @scala.annotation.tailrec
  def addSectionLevelStack(newSectionValues: Seq[Int]): Unit =
    if (embedded) {
      parentExecution match {
        case Some(parent: OpenlawExecutionState) =>
          parent.addSectionLevelStack(newSectionValues)
        case None => sectionLevelStack appendAll newSectionValues
      }
    } else {
      sectionLevelStack appendAll newSectionValues
    }

  def allSectionLevelStack: Seq[Int] =
    if (embedded) {
      parentExecution match {
        case Some(parent: OpenlawExecutionState) =>
          parent.allSectionLevelStack ++ sectionLevelStack
        case None => sectionLevelStack
      }
    } else {
      sectionLevelStack
    }

  def executionLevel: Int = executionLevel(parentExecution, 0)

  def toSerializableInternal: SerializableTemplateExecutionResult = {

    val subExecutionIds = subExecutions.map({
      case (name, execution) => name -> execution.id
    })

    SerializableTemplateExecutionResult(
      id = id,
      info = info,
      templateDefinition = templateDefinition,
      subExecutionIds = subExecutionIds,
      templateExecutions = Map.empty, //will be set later
      executions = executions,
      agreements = agreements,
      variableSectionList = variableSectionList,
      parentExecutionId = parentExecution.map(_.id),
      signatureProofs = signatureProofs,
      variables = variables,
      executedVariables = executedVariablesInternal.distinct.toList,
      mapping = mapping,
      aliases = aliases,
      sectionNameMappingInverse = sectionNameMappingInverse,
      variableTypes = variableTypesInternal.toMap,
      variableSections = variableSectionsInternal
        .map({ case (key, value) => key -> value.toList })
        .toMap,
      parameters = parameters,
      executionType = executionType,
      processedSections = processedSections,
      externalCallStructures = externalCallStructures
    )
  }

  def toSerializable: SerializableTemplateExecutionResult = {

    val templateExecutions =
      getSubExecutions
        .map(e => e.id -> e.toSerializableInternal)
        .toMap

    val current = toSerializableInternal
    val allTemplateExecutions = templateExecutions + (id -> current)

    val allTemplateExecutions2 = allTemplateExecutions.map({
      case (id, e) => id -> e.copy(templateExecutions = allTemplateExecutions)
    })

    current.copy(templateExecutions = allTemplateExecutions.map({
      case (id, executionResult) =>
        id -> executionResult.copy(templateExecutions = allTemplateExecutions2)
    }))
  }

  private def getSubExecutions: List[OpenlawExecutionState] =
    subExecutionsInternal.values.flatMap(e => e :: e.getSubExecutions).toList

  private def executionLevel(
      parent: Option[TemplateExecutionResult],
      acc: Int
  ): Int =
    parent.map(p => executionLevel(p.parentExecution, acc + 1)).getOrElse(acc)

  def registerNewType(
      variableType: VariableType
  ): Result[OpenlawExecutionState] = {
    findVariableType(VariableTypeDefinition(variableType.name)) match {
      case None =>
        variableTypesInternal ++= variableType.typeNames
          .map(name => name -> variableType)
          .toMap
        Success(this)
      case Some(_) =>
        Failure(
          s"the variable type ${variableType.name} as already been defined"
        )
    }
  }

  def notFinished: Boolean = state match {
    case ExecutionFinished => false
    case _                 => true
  }

  def createAnonymousVariable(): VariableName = {
    val currentCounter = anonymousVariableCounter.incrementAndGet()
    VariableName(generateAnonymousName(currentCounter))
  }

  def generateAnonymousName(counter: Int): String = s"@@anonymous_$counter@@"

  def getTemplateIdentifier: Option[TemplateSourceIdentifier] = state match {
    case ExecutionWaitForTemplate(_, identifier, _) =>
      Some(identifier)
    case _ => None
  }

  def getCompiledTemplate(
      templates: Map[TemplateSourceIdentifier, CompiledTemplate]
  ): Option[CompiledTemplate] =
    getTemplateIdentifier flatMap templates.get

  def startForEachExecution(
      variableName: VariableName,
      template: CompiledTemplate,
      name: VariableName,
      value: OpenlawValue,
      varType: VariableType
  ): Result[OpenlawExecutionState] =
    for {
      internalFormat <- varType.internalFormat(value)
      result <- startSubExecution(
        variableName,
        template,
        executionType = BlockExecution,
        Map(name -> internalFormat)
      )
    } yield {
      this.forEachExecutions append result.id
      result
    }

  private def getSectionLevelStack(
      executionType: ExecutionType
  ): mutable.Buffer[Int] = executionType match {
    case TemplateExecution => createConcurrentMutableBuffer
    case _                 => this.sectionLevelStack
  }

  def startSubExecution(
      variableName: VariableName,
      template: CompiledTemplate,
      executionType: ExecutionType,
      overrideParameters: Map[VariableName, String] = Map.empty
  ): Result[OpenlawExecutionState] =
    getVariableValue[TemplateDefinition](variableName).flatMap {
      templateDefinitionOption =>
        templateDefinitionOption
          .map { templateDefinition =>
            detectCyclicDependency(templateDefinition).map(_ => {
              val newExecution = OpenlawExecutionState(
                id = TemplateExecutionResultId(createAnonymousVariable().name),
                info = info,
                parameters = parameters ++ overrideParameters,
                executionType = executionType,
                sectionLevelStack = getSectionLevelStack(executionType),
                template = template,
                parentExecution = Some(this),
                variableRedefinition = template.redefinition,
                templateDefinition = Some(templateDefinition),
                mapping = templateDefinition.mapping,
                remainingElements =
                  createConcurrentMutableBuffer(template.block.elems),
                executions = this.executions
              )

              val execution = template match {
                case agreement: CompiledAgreement =>
                  newExecution.copy(compiledAgreement = Some(agreement))
                case _ =>
                  newExecution
              }

              this.subExecutionsInternal.put(variableName, execution)
              execution
            })
          }
          .getOrElse(
            Failure(
              s"template ${variableName.name} was not resolved! ${variables.map(_.name)}"
            )
          )
    }

  private def detectCyclicDependency(
      definition: TemplateDefinition
  ): Result[TemplateExecutionResult] =
    detectCyclicDependency(this, definition)

  @tailrec
  private def detectCyclicDependency(
      execution: TemplateExecutionResult,
      definition: TemplateDefinition
  ): Result[TemplateExecutionResult] = {
    if (execution.templateDefinition.exists(_ === definition)) {
      Failure(s"cyclic dependency detected on '${definition.name.name}'")
    } else {
      execution.parentExecution match {
        case Some(parent) =>
          detectCyclicDependency(parent, definition)
        case None =>
          Success(execution)
      }
    }
  }

  def structuredMainTemplate(
      agreement: CompiledAgreement,
      overriddenFormatter: (
          Option[FormatterDefinition],
          TemplateExecutionResult
      ) => Option[Formatter]
  ): Result[StructuredAgreement] =
    agreement.structuredMainTemplate(this, overriddenFormatter)

  def structuredInternal(
      agreement: CompiledAgreement,
      overriddenFormatter: (
          Option[FormatterDefinition],
          TemplateExecutionResult
      ) => Option[Formatter]
  ): Result[StructuredAgreement] =
    agreement.structuredInternal(
      this,
      templateDefinition.flatMap(_.path),
      overriddenFormatter
    )

  def findExecutionResultInternal(
      executionResultId: TemplateExecutionResultId
  ): Option[OpenlawExecutionState] = {
    if (id === executionResultId) {
      Some(this)
    } else {
      subExecutionsInternal.values
        .flatMap(_.findExecutionResultInternal(executionResultId))
        .headOption
    }
  }

  def buildStructureValueFromVariables
      : Result[OpenlawMap[VariableName, OpenlawValue]] =
    for {
      values <- variablesThatAreNotTypeDefinition
        .map(variable => variable.evaluate(this).map(variable.name -> _))
        .toList
        .sequence
    } yield OpenlawMap(
      values.flatMap({ case (name, optValue) => optValue.map(name -> _) }).toMap
    )

  private def variablesThatAreNotTypeDefinition
      : mutable.Buffer[VariableDefinition] =
    variablesInternal.filter(_.varType(this) match {
      case AbstractStructureType => false
      case ChoiceType            => false
      case AbstractDomainType    => false
      case _                     => true
    })

  def buildStructureFromVariables: Structure =
    buildStructure(
      variablesThatAreNotTypeDefinition
        .map(variable => variable.name -> variable)
        .toMap
    )

  def buildStructure(
      typeDefinition: Map[VariableName, VariableDefinition]
  ): Structure =
    Structure(
      typeDefinition = typeDefinition,
      names = typeDefinition.keys.toList,
      types = typeDefinition.map({
        case (name, variable) => name -> variable.varType(this)
      })
    )
}

final case class StructuredAgreementId(id: String)

object StructuredAgreement {
  implicit val structuredAgreementEnc: Encoder[StructuredAgreement] =
    deriveEncoder
  implicit val structuredAgreementDec: Decoder[StructuredAgreement] =
    deriveDecoder
}

final case class StructuredAgreement(
    executionResultId: TemplateExecutionResultId,
    templateDefinition: Option[TemplateDefinition],
    mainTemplate: Boolean = false,
    header: TemplateHeader,
    paragraphs: List[Paragraph] = Nil,
    path: Option[TemplatePath] = None
) {
  def title: TemplateTitle = {
    if (header.shouldShowTitle) {
      templateDefinition
        .map(template => template.name.name)
        .getOrElse(TemplateTitle(""))
    } else {
      TemplateTitle()
    }
  }

  def name: String =
    templateDefinition
      .map(template =>
        template.path
          .map(_.path.mkString("/") + "/")
          .getOrElse("") + template.name.name.title
      )
      .getOrElse("")

  def directory: TemplatePath = path.getOrElse(TemplatePath())
}

sealed trait AgreementElement {
  def serialize: Json
}

object AgreementElement {

  private def className[T](implicit classTag: ClassTag[T]): String =
    classTag.runtimeClass.getSimpleName

  implicit val agreementElementEnc: Encoder[AgreementElement] =
    (a: AgreementElement) => {
      Json.obj(
        "name" -> Json.fromString(a.getClass.getSimpleName),
        "value" -> a.serialize
      )
    }
  implicit val agreementElementDec: Decoder[AgreementElement] =
    (c: HCursor) => {
      for {
        name <- c.downField("name").as[String]
        value <- decodeAgreementElement(name, c.downField("value"))
      } yield value
    }

  private def decodeAgreementElement(
      name: String,
      a: ACursor
  ): Decoder.Result[AgreementElement] = {
    name match {
      case _ if name === className[PlainText] =>
        a.as[PlainText]
      case _ if name === className[Title] =>
        a.as[Title]
      case _ if name === className[ImageElement] =>
        a.as[ImageElement]
      case _ if name === className[FreeText] =>
        a.as[FreeText]
      case _ if name === className[Link] =>
        a.as[Link]
      case _ if name === className[VariableElement] =>
        a.as[VariableElement]
      case _ if name === className[SectionElement] =>
        a.as[SectionElement]
      case _ if name === className[ConditionalStart] =>
        a.as[ConditionalStart]
      case _ if name === className[ConditionalEnd] =>
        a.as[ConditionalEnd]
      case _ if name === className[TableElement] =>
        a.as[TableElement]
      case _ if name === className[NoteAnnotation] =>
        a.as[NoteAnnotation]
      case _ if name === className[HeaderAnnotation] =>
        a.as[HeaderAnnotation]
      case _ if name === className[Title] =>
        a.as[Title]
      case _ if name === className[Paragraph] =>
        a.as[Paragraph]
      case _ =>
        Left(DecodingFailure(s"unknown agreement element type $name", Nil))
    }
  }
}

/** Special agreement element type used internally by this printer to demark text to be output without any
  * styling or wrapping elements.
  */
final case class PlainText(str: String) extends AgreementElement {
  override def serialize: Json = this.asJson
}

/** Agreement element to wrap a title to support printing titles in downloaded documents.
  */
final case class Title(title: TemplateTitle) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class ImageElement(url: String) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class FreeText(elem: TextElement) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class SignaturePlaceholder(text: String) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class Link(label: String, url: String) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class VariableElement(
    name: VariableName,
    variableType: Option[VariableType],
    content: List[AgreementElement],
    dependencies: List[String]
) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class SectionElement(
    value: String,
    lvl: Int,
    number: Int,
    resetNumbering: Option[Int],
    overriddenSymbol: Option[SectionSymbol],
    overriddenFormat: Option[SectionFormat]
) extends AgreementElement {
  override def serialize: Json = this.asJson
}
final case class ConditionalStart(dependencies: List[String])
    extends AgreementElement {
  override def serialize: Json = this.asJson
}

final case class ConditionalEnd(dependencies: List[String])
    extends AgreementElement {
  override def serialize: Json = this.asJson
}

final case class ParagraphBuilder(
    paragraphs: List[Paragraph] = Nil,
    lastParagraph: Paragraph = Paragraph()
) {
  def addAllToLastParagraph(
      elements: List[AgreementElement]
  ): ParagraphBuilder =
    elements.foldLeft(this)((builder, element) => builder.add(element))

  def add(element: AgreementElement): ParagraphBuilder =
    this.copy(lastParagraph = lastParagraph.appendElement(element))

  def closeLastParagraph(): ParagraphBuilder =
    this.copy(
      paragraphs = paragraphs :+ lastParagraph,
      lastParagraph = Paragraph()
    )

  def build: List[Paragraph] = paragraphs :+ lastParagraph

}

final case class Paragraph(elements: List[AgreementElement] = Nil)
    extends AgreementElement {
  def appendElement(element: AgreementElement): Paragraph =
    this.copy(elements :+ element)

  override def serialize: Json = this.asJson
}

final case class TableElement(
    header: List[List[AgreementElement]],
    alignment: List[(Alignment, Border)],
    rows: List[List[List[AgreementElement]]]
) extends AgreementElement {
  val rowCount: Int = rows.size
  val columnCount: Int = header.size

  override def serialize: Json = this.asJson
}

final case class HeaderAnnotation(content: String)
    extends TemplatePart
    with AgreementElement {
  override def serialize: Json = this.asJson
}
final case class NoteAnnotation(content: String)
    extends TemplatePart
    with AgreementElement {
  override def serialize: Json = this.asJson
}

final case class DistinctVariableBuilder(
    variables: mutable.Buffer[VariableDefinition] =
      createConcurrentMutableBuffer,
    names: mutable.Set[VariableName] = createConcurrentMutableSet
) {
  def add(variable: VariableDefinition): DistinctVariableBuilder = {
    variables append variable
    names add variable.name
    this
  }
}

object TemplateExecutionState {
  implicit val templateExecutionStateEq: Eq[TemplateExecutionState] =
    Eq.fromUniversalEquals
}

sealed trait TemplateExecutionState
case object ExecutionFinished extends TemplateExecutionState
case object ExecutionReady extends TemplateExecutionState
final case class ExecutionFailed(err: Failure[_]) extends TemplateExecutionState
final case class ExecutionWaitForTemplate(
    variableName: VariableName,
    template: TemplateSourceIdentifier,
    executionType: ExecutionType
) extends TemplateExecutionState

final case class ActionInfo(
    action: ActionValue,
    executionResult: TemplateExecutionResult
) {
  def identifier: Result[ActionIdentifier] = action.identifier(executionResult)
}

object TemplateExecutionResultId {
  implicit val templateExecutionResultIdEq: Eq[TemplateExecutionResultId] =
    Eq.fromUniversalEquals
  implicit val templateExecutionResultIdEnc
      : Encoder[TemplateExecutionResultId] = deriveEncoder
  implicit val templateExecutionResultIdDec
      : Decoder[TemplateExecutionResultId] = deriveDecoder
  implicit val templateExecutionResultIdKeyEnc
      : KeyEncoder[TemplateExecutionResultId] =
    (key: TemplateExecutionResultId) => key.id
  implicit val templateExecutionResultIdKeyDec
      : KeyDecoder[TemplateExecutionResultId] = (key: String) =>
    Some(TemplateExecutionResultId(key))
}

final case class TemplateExecutionResultId(id: String)

final case class ValidationResult(
    identities: List[VariableDefinition],
    missingInputs: List[VariableName],
    missingIdentities: List[VariableName],
    validationExpressionErrors: List[String]
) {
  def successful: Boolean =
    identities.nonEmpty && missingInputs.isEmpty && missingIdentities.isEmpty && validationExpressionErrors.isEmpty
}

object ActionIdentifier {
  implicit val actionIdentifierEnc: Encoder[ActionIdentifier] = deriveEncoder
  implicit val actionIdentifierDec: Decoder[ActionIdentifier] = deriveDecoder
  implicit val actionIdentifierKeyEnc: KeyEncoder[ActionIdentifier] =
    (key: ActionIdentifier) => key.identifier
  implicit val actionIdentifierKeyDec: KeyDecoder[ActionIdentifier] =
    (key: String) => Some(ActionIdentifier(key))
  implicit val actionIdentifierEq: Eq[ActionIdentifier] = Eq.fromUniversalEquals
}

final case class ActionIdentifier(identifier: String)

final case class LazyAgreementData(
    agreement: CompiledAgreement,
    executionResult: TemplateExecutionResult
)
