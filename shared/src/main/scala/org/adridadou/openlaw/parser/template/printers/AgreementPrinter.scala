package org.adridadou.openlaw.parser.template.printers

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.values.TemplateTitle
import cats.implicits._
import org.adridadou.openlaw.parser.template.variableTypes.TemplatePath

trait AgreementPrinter[T] {
  def result: T

  def title(title:TemplateTitle):AgreementPrinter[T]
  def link(link:Link):AgreementPrinter[T]
  def text(txt:TextElement):AgreementPrinter[T]
  def conditionalStart():AgreementPrinter[T] = newState(state.copy(conditionalDepth = state.conditionalDepth + 1 ))
  def conditionalEnd():AgreementPrinter[T] = newState(state.copy(conditionalDepth = state.conditionalDepth - 1 ))
  def conditionalTextStart():AgreementPrinter[T]
  def conditionalTextEnd():AgreementPrinter[T]
  def variableStart(variableName:String):AgreementPrinter[T]
  def variableEnd():AgreementPrinter[T]
  def sectionStart(section:SectionElement):AgreementPrinter[T]
  def sectionHeader(section:SectionElement):AgreementPrinter[T]
  def sectionEnd():AgreementPrinter[T]
  def paragraphHeader(docParagraph:Paragraph):AgreementPrinter[T]
  def paragraphFooter:AgreementPrinter[T]

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

  def generateListNumber(lvl: Int, sections: Seq[Int]): String = {
    val numberInList = calculateNumberInList(lvl, sections)
    lvl match {
      case 1 => lvl1Index(numberInList)
      case 2 => lvl2Index(numberInList)
      case 3 => lvl3Index(numberInList)
      case 4 => lvl4Index(numberInList)
      case _ => throw new RuntimeException("we handle only 4 levels for now")
    }
  }

  def calculateNumberInList(lvl: Int, sections: Seq[Int]): Int =
    sections.reverse.takeWhile(_ >= lvl).count(_ === lvl)

  private def lvl1Index(index: Int) = s"$index."

  private def lvl2Index(index: Int) = s"(${('a' + ((index - 1) % 26)).toChar.toString * ((index - 1) / 26 + 1)})"

  private def lvl3Index(index: Int) = s"(${toRomanNumerals(index)})"

  private def lvl4Index(index: Int) = s"($index)"

  private def toRomanNumerals( number: Int): String = {
    toRomanNumerals( number, List( ("m", 1000),("cm", 900), ("d", 500), ("cd", 400), ("c", 100), ("xc", 90),
      ("l", 50), ("xl",40), ("x", 10), ("ix", 9), ("v", 5), ("iv", 4), ("i", 1) ))
  }

  private def toRomanNumerals( number: Int, digits: List[(String, Int)] ): String = digits match {
    case Nil => ""
    case (str, i) :: t => str * ( number / i ) + toRomanNumerals( number % i, t )
  }
}

/** Type class to support generating output files of varying formats, both singely and in zip files.
  */
trait FileCreator[T] {
  def getMimeType(t: T): String
  def getExtension(t: T): String
  def getBytes(t: T): Array[Byte]
}
object FileCreator {
  def apply[A: FileCreator]: FileCreator[A] = implicitly
  def getMimeType[T: FileCreator](t: T): String = FileCreator[T].getMimeType(t)
  def getExtension[T: FileCreator](t: T): String = FileCreator[T].getExtension(t)
  def getBytes[T: FileCreator](t: T): Array[Byte] = FileCreator[T].getBytes(t)
}

abstract class DocumentGenerator[T : FileCreator] {
  def printer(path:TemplatePath):AgreementPrinter[T]
  def apply(path:TemplatePath, agreementPrinter: AgreementPrinter[T]):DocumentGenerator[T]
  def printers:Seq[(TemplatePath, AgreementPrinter[T])]
}

case class OneDocumentGenerator[T : FileCreator](aggregatedPrinter:AgreementPrinter[T], title:TemplateTitle) extends DocumentGenerator[T] {
  override def printer(path:TemplatePath):AgreementPrinter[T] = aggregatedPrinter
  override def apply(path:TemplatePath, printer: AgreementPrinter[T]):DocumentGenerator[T] = this.copy(aggregatedPrinter = printer.newState(printer.state))
  override def printers:Seq[(TemplatePath, AgreementPrinter[T])] = Seq((TemplatePath(Seq()), aggregatedPrinter))
}

case class MultipleDocumentGenerator[T : FileCreator](printers:Seq[(TemplatePath, AgreementPrinter[T])] = Seq(), constructor: TemplatePath => (TemplatePath, AgreementPrinter[T])) extends DocumentGenerator[T] {

  override def printer(path:TemplatePath):AgreementPrinter[T] = printers.find({case (p,_) => p === path}) match {
    case Some((_, printer)) =>
      printer
    case None =>
      val (_, agreement) = constructor(path)
      agreement
  }

  override def apply(path:TemplatePath, agreementPrinter: AgreementPrinter[T]):DocumentGenerator[T] =
    this.copy(printers = printers ++ Seq((path, agreementPrinter)))
}
