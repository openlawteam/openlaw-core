package org.adridadou.openlaw.parser.template.printers

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.values.TemplateTitle
import cats.implicits._
import org.adridadou.openlaw.result.Result
import org.adridadou.openlaw.result.Implicits.RichOption

trait AgreementPrinter[T] {
  def result: T

  def title(title:TemplateTitle):AgreementPrinter[T]
  def link(link:Link):AgreementPrinter[T]
  def text(txt:TextElement):AgreementPrinter[T]
  def conditionalStart():AgreementPrinter[T] = newState(state.copy(conditionalDepth = state.conditionalDepth + 1 ))
  def conditionalStartWithElse():AgreementPrinter[T] = newState(state.copy(conditionalDepth = state.conditionalDepth + 1 ))
  def conditionalEnd():AgreementPrinter[T] = newState(state.copy(conditionalDepth = state.conditionalDepth - 1 ))
  def conditionalEndWithElse():AgreementPrinter[T] = newState(state.copy(conditionalDepth = state.conditionalDepth - 1 ))
  def conditionalTextStart():AgreementPrinter[T]
  def conditionalTextEnd():AgreementPrinter[T]
  def conditionalTextStartWithElse():AgreementPrinter[T]
  def conditionalTextEndWithElse():AgreementPrinter[T]
  def variableStart(variableName:VariableName):AgreementPrinter[T]
  def variableEnd():AgreementPrinter[T]
  def sectionStart(section:SectionElement):AgreementPrinter[T]
  def sectionHeader(section:SectionElement):AgreementPrinter[T]
  def sectionEnd():AgreementPrinter[T]
  def sectionBreak():AgreementPrinter[T]
  def paragraphHeader(docParagraph:Paragraph):AgreementPrinter[T]
  def paragraphFooter:AgreementPrinter[T]

  def image(image: ImageElement): AgreementPrinter[T]

  def table(table:TableElement)(renderCell:(AgreementElement, AgreementPrinter[T]) => Unit): AgreementPrinter[T]

  def paragraphStart():AgreementPrinter[T] = newState(state.copy(paragraphIndex = state.paragraphIndex + 1, overriddenParagraphGenerated = false))
  def paragraphEnd():AgreementPrinter[T] = this

  def newState(state:PrinterState):AgreementPrinter[T]
  def pageBreak:AgreementPrinter[T]

  val state:PrinterState

  def hasSection(paragraph: Paragraph):Boolean = paragraph.elements.headOption.exists {
    case _: ConditionalStart => paragraph.elements.lift(1) exists hasSection
    case elem => hasSection(elem)
  }

  private def hasSection(elem:AgreementElement):Boolean = elem match {
    case _: SectionElement => true
    case _ => false
  }

  def isCentered(paragraph: Paragraph): Boolean =
    paragraph.elements.headOption.contains(FreeText(Centered))

}

case class PrinterState(
  conditionalDepth:Int = 0,
  paragraphIndex:Int = 0,
  headerGenerated:Boolean = false,
  em:Boolean = false,
  strong:Boolean = false,
  under:Boolean = false,
  sections:Seq[Int] = Seq(),
  overriddenParagraphGenerated:Boolean = false
)

object SectionHelper {

  def closeSections(sections:Seq[Int], builder:StringBuilder):String = {
    sections.lastOption.foldLeft(builder)({
      case (b,lvl) => (0 until lvl).foldLeft(b)({case(b2,_) => b2.append("</li></ul>")} )
    }).toString()
  }

  def handleSections(lvl:Int, sections:Seq[Int], builder:StringBuilder):StringBuilder = {
    val numberInList = calculateNumberInList(lvl, sections)
    (0 until sections.lastOption.map(_ - lvl).getOrElse(0)).foreach(_ => builder.append("</li></ul>"))

    if(numberInList === 0) {
      builder.append(s"<ul class='list-lvl-$lvl'>")
    } else {
      builder.append("</li>")
    }

    builder.append("<li>")

  }

  def generateReferenceValue(lvl: Int, sections: Seq[Int], overrideSymbol: Option[SectionSymbol]): Result[String] = {
    val numberInList = calculateNumberInList(lvl, sections)
    SectionFormats.get(lvl - 1).toResult(s"we handle only ${SectionFormats.size} levels for now").map { case (symbol, _, _, _, _) =>
      formatSectionValue(numberInList, overrideSymbol.getOrElse(symbol), "%s")
    }
  }

  def generateListNumber(lvl: Int, sections: Seq[Int], overrideSymbol: Option[SectionSymbol], overrideFormat: Option[SectionFormat]): Result[String] = {
    val numberInList = calculateNumberInList(lvl, sections)
    SectionFormats.get(lvl - 1).toResult(s"we handle only ${SectionFormats.size} levels for now").map { case (symbol, _, format, _, _) =>
      val sectionSymbol = overrideSymbol.getOrElse(symbol)
      val sectionFormat = overrideFormat.getOrElse(format)
      formatSectionValue(numberInList, sectionSymbol, sectionFormat.formatString)
    }
  }

  def calculateNumberInList(lvl: Int, sections: Seq[Int]): Int =
    sections.reverse.takeWhile(_ >= lvl).count(_ === lvl)

  private def formatSectionValue(index: Int, sectionSymbol: SectionSymbol, formatString: String): String = {
    sectionSymbol match {
      case Decimal => formatString.format(index.toString)
      case LowerLetter => formatString.format(lowerLetter(index))
      case UpperLetter => formatString.format(lowerLetter(index).toUpperCase)
      case LowerRoman => formatString.format(toRomanNumerals(index))
      case UpperRoman => formatString.format(toRomanNumerals(index).toUpperCase)
      case Hide => ""
    }
  }

  private def lowerLetter(index: Int): String = ('a' + ((index - 1) % 26)).toChar.toString * ((index - 1) / 26 + 1)

  private def toRomanNumerals( number: Int): String = {
    toRomanNumerals( number, List( ("m", 1000),("cm", 900), ("d", 500), ("cd", 400), ("c", 100), ("xc", 90),
      ("l", 50), ("xl",40), ("x", 10), ("ix", 9), ("v", 5), ("iv", 4), ("i", 1) ))
  }

  private def toRomanNumerals( number: Int, digits: List[(String, Int)] ): String = digits match {
    case Nil => ""
    case (str, i) :: t => str * ( number / i ) + toRomanNumerals( number % i, t )
  }
}

