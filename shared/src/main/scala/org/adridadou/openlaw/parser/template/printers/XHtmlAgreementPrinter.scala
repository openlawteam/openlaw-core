package org.adridadou.openlaw.parser.template.printers

import java.util.concurrent.atomic.AtomicInteger

import cats.implicits._
import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.IdentityType
import org.adridadou.openlaw.values.TemplateTitle
import scalatags.Text.all._
import slogging._

import scala.annotation.tailrec

/** Special agreement element type used internally by this printer to demark text to be output without any
  * styling or wrapping elements.
  */
case class PlainText(str: String) extends AgreementElement

/** Agreement element to wrap a title to support printing titles in downloaded documents.
  */
case class Title(title: TemplateTitle) extends AgreementElement

object XHtmlAgreementPrinter {

  /** Implicit value class to enrich the Seq[Frag] with a function to print
    * it's content as an XHTML string.
    */
  implicit class FragsPrinter(val frags: Seq[Frag]) extends AnyVal {
    def print: String = frags.map(_.toString).fold("")(_ + _)
  }
}

case class XHtmlAgreementPrinter(preview: Boolean, paragraphEdits: ParagraphEdits = ParagraphEdits(), hiddenVariables: Seq[String] = Seq()) extends LazyLogging {

  private def partitionAtItem[T](seq: Seq[T], t: T): (Seq[T], Seq[T]) = partitionAt(seq) { case item if item.equals(t) => true }

  private def partitionAt[T](seq: Seq[T])(pf: PartialFunction[T, Boolean]): (Seq[T], Seq[T]) = {
    seq.prefixLength { x => (!pf.isDefinedAt(x)) || !pf(x) } match {
      case 0 => (seq, Nil)
      case length => seq.take(length) -> seq.drop(length)
    }
  }

  // separate content for each of the sections at this level
  private def partitionSections(level: Int, seq: Seq[AgreementElement]): Seq[(SectionElement, Seq[AgreementElement])] = seq match {
    case Seq() => Seq()
    case Seq(section: SectionElement, xs @ _*) =>
      val (content, remaining) = partitionAt(seq.drop(1)) { case SectionElement(_, thisLevel, _, _, _, _) if thisLevel === level => true }
      (section -> content) +: partitionSections(level, remaining)
  }

  @tailrec private def addBreaks(remaining: Seq[Frag], result: Seq[Frag] = Seq()): Seq[Frag] = remaining match {
    case Seq() => result
    case Seq(x) => result :+ x
    case Seq(x, xs @ _*) => addBreaks(xs, result :+ x :+ br())
  }

  private def text(str: String): Seq[Frag] = addBreaks(str.split("\n", -1).map(stringFrag))

  private[this] val paragraphCounter = new AtomicInteger()

  def printRoot(paragraphs: Seq[Paragraph]): String =
    html(
      body(printParagraphs(paragraphs))
    ).toString

  def printParagraphs(paragraphs: Seq[Paragraph]): Seq[Frag] = {
    printFragments(paragraphs, 0, inSection = false)
  }

  def printFragments(elements: Seq[AgreementElement], conditionalBlockDepth: Int, inSection: Boolean): Seq[Frag] = {
    // Local reference to same function with the current arguments as defaults, sugar for simplifying recursive calls
    def recurse(elements: Seq[AgreementElement], conditionalBlockDepth: Int = conditionalBlockDepth, inSection: Boolean = inSection) =
      printFragments(elements, conditionalBlockDepth, inSection)

    elements match {
      case Seq() =>
        Seq()

      case Seq(x, xs @ _*) => x match {

        // If this paragraph contains a section definition, extract all following sections for processing together
        case p @ Paragraph(Seq(s: SectionElement, _*)) =>

          val fixed = elements map {
            case Paragraph(Seq(s: SectionElement, xs @ _*)) => Seq(s, Paragraph(PlainText(s.value + " ") :: xs.toList))
            case x => Seq(x)
          }

          recurse(fixed.flatten)

        case p @ Paragraph(Seq(c: ConditionalStart, remaining @ _*)) =>
          recurse(c +: Paragraph(remaining.toList) +: xs)
        case p @ Paragraph(Seq(c: ConditionalStartWithElse, remaining @ _*)) =>
          recurse(c +: Paragraph(remaining.toList) +: xs)

        case Paragraph(Seq()) =>
          recurse(xs)

        case Paragraph(paragraphElements) =>
          val paragraphCount = paragraphCounter.incrementAndGet()

          // Generate overridden paragraph contents if required
          val overridden = paragraphEdits.edits.get(paragraphCount - 1) match {

            // If there is an overridden paragraph, render its content instead of this paragraph
            case Some(str) =>
              val results = MarkdownParser.parseMarkdownOrThrow(str)
              results.map(FreeText)

            // Otherwise, render this paragraph as normal
            case None =>
              paragraphElements
          }

          // See if this paragraph is centered
          val (align, remaining) = overridden match {
            case Seq(FreeText(Centered), xs @ _*) => (Seq("align-center"), xs)
            case Seq(FreeText(Indent), xs @ _*) => (Seq("indent"), xs)
            case Seq(FreeText(RightAlign), xs @ _*) => (Seq("align-right"), xs)
            case Seq(FreeText(RightThreeQuarters), xs @ _*) => (Seq("align-right-three-quarters"), xs)
            case seq => (Seq(), seq)
          }

          // Setup classes to be added to this paragraph element
          val classes = Seq() ++ (if (!inSection) Seq("no-section") else Nil) ++ align

          val paragraph = if (classes.isEmpty) {
            p(recurse(remaining))
          } else {
            p(`class` := classes.mkString(" "))(recurse(remaining))
          }

          // Recurse on the paragraph contents to render them
          val innerFrag = if (preview) {
            div(`class` := s"openlaw-paragraph paragraph-$paragraphCount")(paragraph)
          } else {
            paragraph
          }
          innerFrag +: recurse(xs)


        case t: TableElement =>
          val frag = table(`class` := "markdown-table")(
            tr(`class` := "markdown-table-row")(
              t.header.map { tableElements =>
                th(`class` := "markdown-table-header")(recurse(tableElements))
              }
            ),
            t.rows.map { row =>
              tr(`class` := "markdown-table-row")(
                row.map { tableElements =>
                  td(`class` := "markdown-table-data")(recurse(tableElements))
                }
              )
            }
          )
          frag +: recurse(xs)

        case section@SectionElement(value, level, resetNumbering, _, _, _) =>
          // partition out all content that will be within the newly defined sections
          val higherLevel = level - 1
          val (content, remaining) = partitionAt(xs) { case SectionElement(_, thisLevel, _, _, _, _) if thisLevel === higherLevel => true }

          // Partition the elements into sections at this level
          val sections = partitionSections(level, section +: content)

          val frag = ul(`class` := s"list-lvl-$level")(
            sections.map { section =>
              li(recurse(section._2, inSection = true))
            }
          )
          frag +: recurse(remaining)

        case VariableElement(name, variableType, content, dependencies) =>
          // Do not highlight identity variables
          val highlightType = variableType.map(_ =!= IdentityType).getOrElse(true)

          // Only add styling to highlight variable if there are no hidden variables that are dependencies for this one
          val frags = if (highlightType && preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) {
            val nameClass = name.replace(" ", "-")
            Seq(span(`class` := s"markdown-variable markdown-variable-$nameClass")(recurse(content)))
          } else {
            recurse(content)
          }
          frags ++ recurse(xs)

        case ImageElement(url) =>
          img(`class` := "markdown-embedded-image", src := url) +: recurse(xs)

        case ConditionalStart(dependencies) =>
          val addDepth = if (preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) 1 else 0
          recurse(xs, conditionalBlockDepth = conditionalBlockDepth + addDepth)

        case ConditionalEnd(dependencies) =>
          val removeDepth = if (preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) 1 else 0
          recurse(xs, conditionalBlockDepth = conditionalBlockDepth - removeDepth)

        case ConditionalStartWithElse(dependencies) =>
          val addDepth = if (preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) 1 else 0
          recurse(xs, conditionalBlockDepth = conditionalBlockDepth + addDepth)

        case ConditionalEndWithElse(dependencies) =>
          val removeDepth = if (preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) 1 else 0
          recurse(xs, conditionalBlockDepth = conditionalBlockDepth - removeDepth)

        case Link(label, url) =>
          a(href := url)(label) +: recurse(xs)

        case PlainText(str) =>
          text(str) ++ recurse(xs)

        case FreeText(t: Text) =>

          // Consume any following text elements which are only newlines
          xs.dropWhile { case element => element === FreeText(Text("\n")) }

          // Generate text output
          val innerFrag = text(t.str)

          val spanFrag: Seq[Frag] = if (conditionalBlockDepth > 0) {
            Seq(span(`class` := "markdown-conditional-block")(innerFrag))
          } else {
            innerFrag
          }

          spanFrag ++ recurse(xs)

        case FreeText(Em) =>
          val (inner, remaining) = partitionAtItem(xs, x)
          val frag = em(recurse(inner))
          frag +: recurse(remaining.drop(1))

        case FreeText(Strong) =>
          val (inner, remaining) = partitionAtItem(xs, x)
          val frag = strong(recurse(inner))
          frag +: recurse(remaining.drop(1))

        case FreeText(PageBreak) =>
          hr(`class` := "pagebreak") +: recurse(xs)

        case FreeText(Centered) =>
          recurse(xs)

        case TopAnnotation(content) =>
          if(preview) {
            span(`class` := "openlaw-annotation-top")(text(content)) +: recurse(xs)
          } else {
            recurse(xs)
          }

        case MiddleAnnotation(content) =>
          if(preview) {
            span(`class` := "openlaw-annotation-middle")(text(content)) +: recurse(xs)
          } else {
            recurse(xs)
          }

        case Title(title) =>
          h1(`class` := "signature-title")(title.title) +: recurse(xs)

        case x =>
          logger.warn(s"unhandled element: $x")
          recurse(xs)
      }
    }
  }
}
