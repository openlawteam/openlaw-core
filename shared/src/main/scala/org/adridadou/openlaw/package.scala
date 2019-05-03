package org.adridadou

import java.time.LocalDateTime

import org.adridadou.openlaw.oracles.EthereumEventFilterExecution
import org.adridadou.openlaw.parser.template.{AgreementElement, ListParameter, MappingParameter, OneValueParameter, Parameter, Parameters}
import org.adridadou.openlaw.parser.template.variableTypes._
import simulacrum._

package object openlaw {

  /*
  trait OpenlawValue extends Comparable[OpenlawValue] {
    type T <: Comparable[T]
    def get: T

    override def compareTo(t: OpenlawValue): Int = get.compareTo(t)
  }
  */

  trait OpenlawValue  {
    type T
    def get: T
  }

  implicit class StringOpenlawValue(val v: String) extends OpenlawValue {
    type T = String

    def get = v
  }

  implicit class ParameterOpenlawValue(val v: Parameter) extends OpenlawValue {
    type T = Parameter

    def get = v
  }

  implicit class SeqOpenlawValue[AgreementElement](val v: Seq[AgreementElement]) extends OpenlawValue {
    type T = Seq[AgreementElement]

    def get = v
  }

  implicit class AddressOpenlawValue(val v: Address) extends OpenlawValue {
    type T = Address

    def get = v
  }

  implicit class ChoicesOpenlawValue(val v: Choices) extends OpenlawValue {
    type T = Choices

    def get = v
  }

  implicit class MapOpenlawValue[X, Y](val v: Map[X, Y]) extends OpenlawValue {
    type T = Map[X, Y]

    def get = v
  }

  implicit class DateTimeTypeOpenlawValue(val v: DateTimeType.type ) extends OpenlawValue {
    type T = DateTimeType.type
    def get = v
  }

  implicit class DateTypeOpenlawValue(val v: DateType.type ) extends OpenlawValue {
    type T = DateType.type
    def get = v
  }

  implicit class IntOpenlawValue(val v: Int) extends OpenlawValue {
    type T = Int
    def get = v
  }

  implicit class IntegerOpenlawValue(val v: Integer) extends OpenlawValue {
    type T = Integer
    def get = v
  }

  implicit class EthereumAddressOpenlawValue(val v: EthereumAddress) extends OpenlawValue {
    type T = EthereumAddress
    def get = v
  }

  implicit class EthereumHashOpenlawValue(val v: EthereumHash) extends OpenlawValue {
    type T = EthereumHash
    def get = v
  }

  implicit class EthereumSmartContractCallOpenlawValue(val v: EthereumSmartContractCall) extends OpenlawValue {
    type T = EthereumSmartContractCall
    def get = v
  }

  implicit class EthereumEventFilterExecutionOpenlawValue(val v: EthereumEventFilterExecution) extends OpenlawValue {
    type T = EthereumEventFilterExecution
    def get = v
  }

  implicit class EventFilterDefinitionOpenlawValue(val v: EventFilterDefinition) extends OpenlawValue {
    type T = EventFilterDefinition
    def get = v
  }

  implicit class LocalDateTimeOpenlawValue(val v: LocalDateTime) extends OpenlawValue {
    type T = LocalDateTime
    def get = v
  }

  implicit class OpenlawExecutionOpenlawValue(val v: OpenlawExecution) extends OpenlawValue {
    type T = OpenlawExecution
    def get = v
  }

  implicit class EthereumSmartContractExecutionOpenlawValue(val v: EthereumSmartContractExecution) extends OpenlawValue {
    type T = EthereumSmartContractExecution
    def get = v
  }

  implicit class StructureOpenlawValue(val v: Structure) extends OpenlawValue {
    type T = Structure
    def get = v
  }

  implicit class BigDecimalOpenlawValue(val v: BigDecimal) extends OpenlawValue {
    type T = BigDecimal
    def get = v
  }

  implicit class PeriodOpenlawValue(val v: Period) extends OpenlawValue {
    type T = Period
    def get = v
  }

  implicit class SectionInfoOpenlawValue(val v: SectionInfo) extends OpenlawValue {
    type T = SectionInfo
    def get = v
  }

  implicit class SmartContractMetadataOpenlawValue(val v: SmartContractMetadata) extends OpenlawValue {
    type T = SmartContractMetadata
    def get = v
  }

  implicit class MappingParameterOpenlawValue(val v: MappingParameter) extends OpenlawValue {
    type T = MappingParameter
    def get = v
  }

  implicit class ParametersOpenlawValue(val v: Parameters) extends OpenlawValue {
    type T = Parameters
    def get = v
  }

  implicit class ListParameterOpenlawValue(val v: ListParameter) extends OpenlawValue {
    type T = ListParameter
    def get = v
  }

  implicit class OneValueParameterOpenlawValue(val v: OneValueParameter) extends OpenlawValue {
    type T = OneValueParameter
    def get = v
  }

  implicit class BooleanOpenlawValue(val v: Boolean) extends OpenlawValue {
    type T = Boolean
    def get = v
  }

  implicit class CollectionValueOpenlawValue(val v: CollectionValue) extends OpenlawValue {
    type T = CollectionValue
    def get = v
  }

  implicit class TemplateDefinitionOpenlawValue(val v: TemplateDefinition) extends OpenlawValue {
    type T = TemplateDefinition
    def get = v
  }

  implicit class IdentityOpenlawValue(val v: Identity) extends OpenlawValue {
    type T = Identity
    def get = v
  }

  implicit class ValidationOpenlawValue(val v: Validation) extends OpenlawValue {
    type T = Validation
    def get = v
  }

  implicit class TemplatePathOpenlawValue(val v: TemplatePath) extends OpenlawValue {
    type T = TemplatePath
    def get = v
  }
}
