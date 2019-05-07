package org.adridadou

import java.time.LocalDateTime

import org.adridadou.openlaw.oracles.EthereumEventFilterExecution
import org.adridadou.openlaw.parser.template.{AgreementElement, ListParameter, MappingParameter, OneValueParameter, Parameter, Parameters}
import org.adridadou.openlaw.parser.template.variableTypes.{DateTimeType, DateType, _}
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
    implicit def tConversion(t: T): this.type
  }

  implicit class StringOpenlawValue(val v: String) extends OpenlawValue {
    type T = String

    def get = v

    override implicit def tConversion(t: String): StringOpenlawValue.this.type = this
  }

  implicit class ParameterOpenlawValue(val v: Parameter) extends OpenlawValue {
    type T = Parameter

    def get = v

    override implicit def tConversion(t: Parameter): ParameterOpenlawValue.this.type = this
  }

  implicit class SeqOpenlawValue[AgreementElement](val v: Seq[AgreementElement]) extends OpenlawValue {
    type T = Seq[AgreementElement]

    def get = v

    override implicit def tConversion(t: Seq[AgreementElement]): SeqOpenlawValue.this.type = this
  }

  implicit class AddressOpenlawValue(val v: Address) extends OpenlawValue {
    type T = Address

    def get = v

    override implicit def tConversion(t: Address): AddressOpenlawValue.this.type = this
  }

  implicit class ChoicesOpenlawValue(val v: Choices) extends OpenlawValue {
    type T = Choices

    def get = v

    override implicit def tConversion(t: Choices): ChoicesOpenlawValue.this.type = this
  }

  implicit class MapOpenlawValue[X, Y](val v: Map[X, Y]) extends OpenlawValue {
    type T = Map[X, Y]

    def get = v

    override implicit def tConversion(t: Map[X, Y]): MapOpenlawValue.this.type = this
  }

  implicit class DateTimeTypeOpenlawValue(val v: DateTimeType.type ) extends OpenlawValue {
    type T = DateTimeType.type
    def get = v

    override implicit def tConversion(t: DateTimeType.type): DateTimeTypeOpenlawValue.this.type = this
  }

  implicit class DateTypeOpenlawValue(val v: DateType.type ) extends OpenlawValue {
    type T = DateType.type
    def get = v

    override implicit def tConversion(t: DateType.type): DateTypeOpenlawValue.this.type = this
  }

  implicit class IntOpenlawValue(val v: Int) extends OpenlawValue {
    type T = Int
    def get = v

    override implicit def tConversion(t: Int): IntOpenlawValue.this.type = this
  }

  implicit class IntegerOpenlawValue(val v: Integer) extends OpenlawValue {
    type T = Integer
    def get = v

    override implicit def tConversion(t: Integer): IntegerOpenlawValue.this.type = this
  }

  implicit class EthereumAddressOpenlawValue(val v: EthereumAddress) extends OpenlawValue {
    type T = EthereumAddress
    def get = v

    override implicit def tConversion(t: EthereumAddress): EthereumAddressOpenlawValue.this.type = this
  }

  implicit class EthereumHashOpenlawValue(val v: EthereumHash) extends OpenlawValue {
    type T = EthereumHash
    def get = v

    override implicit def tConversion(t: EthereumHash): EthereumHashOpenlawValue.this.type = this
  }

  implicit class EthereumSmartContractCallOpenlawValue(val v: EthereumSmartContractCall) extends OpenlawValue {
    type T = EthereumSmartContractCall
    def get = v

    override implicit def tConversion(t: EthereumSmartContractCall): EthereumSmartContractCallOpenlawValue.this.type = this
  }

  implicit class EthereumEventFilterExecutionOpenlawValue(val v: EthereumEventFilterExecution) extends OpenlawValue {
    type T = EthereumEventFilterExecution
    def get = v

    override implicit def tConversion(t: EthereumEventFilterExecution): EthereumEventFilterExecutionOpenlawValue.this.type = this
  }

  implicit class EventFilterDefinitionOpenlawValue(val v: EventFilterDefinition) extends OpenlawValue {
    type T = EventFilterDefinition
    def get = v

    override implicit def tConversion(t: EventFilterDefinition): EventFilterDefinitionOpenlawValue.this.type = this
  }

  implicit class LocalDateTimeOpenlawValue(val v: LocalDateTime) extends OpenlawValue {
    type T = LocalDateTime
    def get = v

    override implicit def tConversion(t: LocalDateTime): LocalDateTimeOpenlawValue.this.type = this
  }

  implicit class OpenlawExecutionOpenlawValue(val v: OpenlawExecution) extends OpenlawValue {
    type T = OpenlawExecution
    def get = v

    override implicit def tConversion(t: OpenlawExecution): OpenlawExecutionOpenlawValue.this.type = this
  }

  implicit class EthereumSmartContractExecutionOpenlawValue(val v: EthereumSmartContractExecution) extends OpenlawValue {
    type T = EthereumSmartContractExecution
    def get = v

    override implicit def tConversion(t: EthereumSmartContractExecution): EthereumSmartContractExecutionOpenlawValue.this.type = this
  }

  implicit class StructureOpenlawValue(val v: Structure) extends OpenlawValue {
    type T = Structure
    def get = v

    override implicit def tConversion(t: Structure): StructureOpenlawValue.this.type = this
  }

  implicit class BigDecimalOpenlawValue(val v: BigDecimal) extends OpenlawValue {
    type T = BigDecimal
    def get = v

    override implicit def tConversion(t: BigDecimal): BigDecimalOpenlawValue.this.type = this
  }

  implicit class PeriodOpenlawValue(val v: Period) extends OpenlawValue {
    type T = Period
    def get = v

    override implicit def tConversion(t: Period): PeriodOpenlawValue.this.type = this
  }

  implicit class SectionInfoOpenlawValue(val v: SectionInfo) extends OpenlawValue {
    type T = SectionInfo
    def get = v

    override implicit def tConversion(t: SectionInfo): SectionInfoOpenlawValue.this.type = this
  }

  implicit class SmartContractMetadataOpenlawValue(val v: SmartContractMetadata) extends OpenlawValue {
    type T = SmartContractMetadata
    def get = v

    override implicit def tConversion(t: SmartContractMetadata): SmartContractMetadataOpenlawValue.this.type = this
  }

  implicit class MappingParameterOpenlawValue(val v: MappingParameter) extends OpenlawValue {
    type T = MappingParameter
    def get = v

    override implicit def tConversion(t: MappingParameter): MappingParameterOpenlawValue.this.type = this
  }

  implicit class ParametersOpenlawValue(val v: Parameters) extends OpenlawValue {
    type T = Parameters
    def get = v

    override implicit def tConversion(t: Parameters): ParametersOpenlawValue.this.type = this
  }

  implicit class ListParameterOpenlawValue(val v: ListParameter) extends OpenlawValue {
    type T = ListParameter
    def get = v

    override implicit def tConversion(t: ListParameter): ListParameterOpenlawValue.this.type = this
  }

  implicit class OneValueParameterOpenlawValue(val v: OneValueParameter) extends OpenlawValue {
    type T = OneValueParameter
    def get = v

    override implicit def tConversion(t: OneValueParameter): OneValueParameterOpenlawValue.this.type = this
  }

  implicit class BooleanOpenlawValue(val v: Boolean) extends OpenlawValue {
    type T = Boolean
    def get = v

    override implicit def tConversion(t: Boolean): BooleanOpenlawValue.this.type = this
  }

  implicit class CollectionValueOpenlawValue(val v: CollectionValue) extends OpenlawValue {
    type T = CollectionValue
    def get = v

    override implicit def tConversion(t: CollectionValue): CollectionValueOpenlawValue.this.type = this
  }

  implicit class TemplateDefinitionOpenlawValue(val v: TemplateDefinition) extends OpenlawValue {
    type T = TemplateDefinition
    def get = v

    override implicit def tConversion(t: TemplateDefinition): TemplateDefinitionOpenlawValue.this.type = this
  }

  implicit class IdentityOpenlawValue(val v: Identity) extends OpenlawValue {
    type T = Identity
    def get = v

    override implicit def tConversion(t: Identity): IdentityOpenlawValue.this.type = this
  }


  implicit class ValidationOpenlawValue(val v: Validation) extends OpenlawValue {
    type T = Validation
    def get = v

    override implicit def tConversion(t: Validation): ValidationOpenlawValue.this.type = this
  }

  implicit class TemplatePathOpenlawValue(val v: TemplatePath) extends OpenlawValue {
    type T = TemplatePath
    def get = v

    override implicit def tConversion(t: TemplatePath): TemplatePathOpenlawValue.this.type = this
  }
}
