package org.adridadou.openlaw.parser

import cats.Eq

package object template {

  implicit val agreementElementEq: Eq[AgreementElement] = Eq.fromUniversalEquals

  implicit val parametersEq: Eq[Parameters] = Eq.fromUniversalEquals

  implicit val sectionDefinitionEq: Eq[SectionDefinition] =
    Eq.fromUniversalEquals

  implicit val sectionEq: Eq[Section] = Eq.fromUniversalEquals

  val SectionFormats
      : List[(SectionSymbol, SectionAlign, SectionFormat, Int, Int)] =
    (Decimal, AlignLeft, Period, 0, 360) ::
      (LowerLetter, AlignLeft, Parens, 360, 360) ::
      (LowerRoman, AlignLeft, Parens, 640, 360) ::
      (Decimal, AlignLeft, Parens, 960, 360) ::
      (LowerLetter, AlignLeft, Parens, 1320, 360) ::
      (LowerRoman, AlignLeft, Parens, 1680, 360) ::
      (Decimal, AlignLeft, Period, 2040, 360) ::
      (LowerLetter, AlignLeft, Parens, 2400, 360) ::
      (LowerRoman, AlignLeft, Parens, 2760, 360) ::
      Nil

  object SectionSymbol {
    val values: Seq[SectionSymbol] =
      Seq(Decimal, LowerLetter, UpperLetter, LowerRoman, UpperRoman, Hide)
    def withNameOption(name: String): Option[SectionSymbol] =
      values.find(_.toString.toLowerCase.equals(name.toLowerCase))
  }
  sealed trait SectionSymbol
  case object Decimal extends SectionSymbol
  case object LowerLetter extends SectionSymbol
  case object UpperLetter extends SectionSymbol
  case object LowerRoman extends SectionSymbol
  case object UpperRoman extends SectionSymbol
  case object Hide extends SectionSymbol

  object SectionFormat {
    val values: Seq[SectionFormat] = Seq(Period, Parens, RightParen)
    def withNameOption(name: String): Option[SectionFormat] =
      values.find(_.toString.toLowerCase.equals(name.toLowerCase))
  }
  sealed trait SectionFormat {
    def formatString: String = this match {
      case Period     => "%s."
      case Parens     => "(%s)"
      case RightParen => "%s)"
    }
  }
  case object Period extends SectionFormat
  case object Parens extends SectionFormat
  case object RightParen extends SectionFormat

  object SectionAlign {
    val values: Seq[SectionAlign] =
      Seq(AlignLeft, AlignRight, AlignRightThreeQuarters)
    def withNameOption(name: String): Option[SectionAlign] =
      values.find(_.toString.toLowerCase.equals(name.toLowerCase))
  }
  sealed trait SectionAlign
  case object AlignLeft extends SectionAlign
  case object AlignRight extends SectionAlign
  case object AlignRightThreeQuarters extends SectionAlign
}
