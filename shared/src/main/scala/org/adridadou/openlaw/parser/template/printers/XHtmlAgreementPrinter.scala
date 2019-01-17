package org.adridadou.openlaw.parser.template.printers

import java.util.concurrent.atomic.AtomicInteger

import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.IdentityType
import org.adridadou.openlaw.values.TemplateTitle
import scalatags.Text.all._
import slogging._

import scala.annotation.tailrec

import cats.Eval
import cats.implicits._

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

  private def partitionAtItem[T](seq: List[T], t: T): (List[T], List[T]) = partitionAt(seq) { case item if item.equals(t) => true }

  private def partitionAt[T](seq: List[T])(pf: PartialFunction[T, Boolean]): (List[T], List[T]) = {
    seq.prefixLength { x => (!pf.isDefinedAt(x)) || !pf(x) } match {
      case 0 => (seq, Nil)
      case length => seq.take(length) -> seq.drop(length)
    }
  }

  // separate content for each of the sections at this level
  private def partitionSections(level: Int, seq: List[AgreementElement]): List[(SectionElement, List[AgreementElement])] = seq match {
    case (section: SectionElement) :: _ =>
      val (content, remaining) = partitionAt(seq.drop(1)) { case SectionElement(_, thisLevel, _, _, _, _) if thisLevel === level => true }
      (section -> content) +: partitionSections(level, remaining)
    case _ => List()
  }

  @tailrec private def addBreaks(remaining: List[Frag], result: List[Frag] = Nil): List[Frag] = remaining match {
    case Nil => result
    case x :: Nil => result :+ x
    case x :: xs => addBreaks(xs, result :+ x :+ br())
  }

  private def text(str: String): List[Frag] = addBreaks(str.split("\n", -1).map(stringFrag).toList)

  private[this] val paragraphCounter = new AtomicInteger()

  def printRoot(paragraphs: List[Paragraph]): String =
    html(
      body(printParagraphs(paragraphs))
    ).toString

  def printParagraphs(paragraphs: List[Paragraph]): List[Frag] = {
    printFragments(paragraphs, 0, inSection = false)
  }

  def printFragments(elements: List[AgreementElement], conditionalBlockDepth: Int, inSection: Boolean): List[Frag] = {
    printFragmentsInternal(elements, conditionalBlockDepth, inSection).value
  }

  def printFragmentsInternal(elements: List[AgreementElement], conditionalBlockDepth: Int, inSection: Boolean): Eval[List[Frag]] = {
    // Local reference to same function with the current arguments as defaults, sugar for simplifying recursive calls
    def recurse(elements: List[AgreementElement], conditionalBlockDepth: Int = conditionalBlockDepth, inSection: Boolean = inSection): Eval[List[Frag]] =
      printFragmentsInternal(elements, conditionalBlockDepth, inSection)

    def printParagraphWithSection(elements: List[AgreementElement]): Eval[List[Frag]] = {
      val fixed = elements flatMap {
        case Paragraph((s: SectionElement) :: rest) => s :: Paragraph(PlainText(s.value + " ") :: rest) :: Nil
        case x => List(x)
      }
      recurse(fixed)
    }

    def printParagraph(paragraphElements: List[AgreementElement], xs: List[AgreementElement]): Eval[List[Frag]] = {
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
        case FreeText(Centered) :: r => (List("align-center"), r)
        case FreeText(Indent) :: r => (List("indent"), r)
        case FreeText(RightAlign) :: r => (List("align-right"), r)
        case FreeText(RightThreeQuarters) :: r => (List("align-right-three-quarters"), r)
        case seq => (List(), seq)
      }

      // Setup classes to be added to this paragraph element
      val classes = Seq() ++ (if (!inSection) Seq("no-section") else Nil) ++ align

      val lazyParagraph = if (classes.isEmpty) {
        recurse(remaining).map(elems => p(elems))
      } else {
        recurse(remaining).map(elems => p(`class` := classes.mkString(" "))(elems))
      }

      // Recurse on the paragraph contents to render them
      val lazyInnerFrag = if (preview) {
        lazyParagraph.map(paragraph => div(`class` := s"openlaw-paragraph paragraph-$paragraphCount")(paragraph))
      } else {
        lazyParagraph
      }

      for {
        innerFrag <- lazyInnerFrag
        elems <- recurse(xs)
      } yield innerFrag +: elems
    }

    def printTableElement(t: TableElement, xs: List[AgreementElement]): Eval[List[Frag]] = {
      val lazyHeader = t.header.map(tableElements => recurse(tableElements)).sequence
      val lazyRows = t.rows.map(row => row.map(tableElements => recurse(tableElements)).sequence).sequence

      val lazyFrag = for {
        header <- lazyHeader
        rows <- lazyRows
      } yield {
        table(`class` := "markdown-table")(
          tr(`class` := "markdown-table-row")(
            header.map { tableElements =>
              th(`class` := "markdown-table-header")(tableElements)
            }
          ),
          rows.map { row =>
            tr(`class` := "markdown-table-row")(
              row.map { tableElements =>
                td(`class` := "markdown-table-data")(tableElements)
              }
            )
          }
        )
      }

      for {
        elems <- recurse(xs)
        frag <- lazyFrag
      } yield frag +: elems
    }

    elements match {
      case Nil =>
        Eval.now {
          Nil
        }

      case x :: xs => x match {

        // If this paragraph contains a section definition, extract all following sections for processing together
        case Paragraph((_: SectionElement) :: _) =>
          printParagraphWithSection(elements)
        case Paragraph((c: ConditionalStart) :: remaining) =>
          recurse(c :: Paragraph(remaining) :: xs)
        case Paragraph((c: ConditionalStartWithElse) :: remaining) =>
          recurse(c +: Paragraph(remaining) +: xs)

        case Paragraph(Nil) =>
          recurse(xs)

        case Paragraph(paragraphElements) =>
          printParagraph(paragraphElements, xs)

        case t: TableElement =>
          printTableElement(t, xs)

        case section@SectionElement(_, level, _, _, _, _) =>
          // partition out all content that will be within the newly defined sections
          val higherLevel = level - 1
          val (content, remaining) = partitionAt(xs) { case SectionElement(_, thisLevel, _, _, _, _) if thisLevel === higherLevel => true }

          // Partition the elements into sections at this level
          val sections = partitionSections(level, section +: content)

          val sectionsFrag = sections.map { case (_, sectionElements) =>
            recurse(sectionElements, inSection = true).map(elems => li(elems))
          }.sequence

          val lazyFrag = sectionsFrag.map(elems => ul(`class` := s"list-lvl-$level")(elems))

          for {
            frag <- lazyFrag
            remainingElems <- recurse(remaining)
          } yield frag :: remainingElems

        case VariableElement(name, variableType, content, dependencies) =>
          // Do not highlight identity variables
          val highlightType = variableType.forall(_ =!= IdentityType)

          // Only add styling to highlight variable if there are no hidden variables that are dependencies for this one
          val lazyFrags = if (highlightType && preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) {
            val nameClass = name.replace(" ", "-")
            recurse(content).map(elems => List(span(`class` := s"markdown-variable markdown-variable-$nameClass")(elems)))
          } else {
            recurse(content)
          }

          for {
            frags <- lazyFrags
            elems <- recurse(xs)
          } yield frags ++ elems

        case ImageElement(url) =>
          recurse(xs).map(elems => img(`class` := "markdown-embedded-image", src := url) :: elems)

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
          recurse(xs).map(a(href := url)(label) :: _)

        case PlainText(str) =>
          recurse(xs).map(elems => text(str) ++ elems)

        case FreeText(t: Text) =>

          // Consume any following text elements which are only newlines
          xs.dropWhile(_ === FreeText(Text("\n")))

          // Generate text output
          val innerFrag = text(t.str)

          val spanFrag: List[Frag] = if (conditionalBlockDepth > 0) {
            List(span(`class` := "markdown-conditional-block")(innerFrag))
          } else {
            innerFrag
          }

          recurse(xs).map(elems => spanFrag ++ elems)

        case FreeText(Em) =>
          val (inner, remaining) = partitionAtItem(xs, x)

          for {
            innerElements <- recurse(inner)
            elements <- recurse(remaining.drop(1))
          } yield em(innerElements) :: elements

        case FreeText(Strong) =>
          val (inner, remaining) = partitionAtItem(xs, x)

          for {
            innerElements <- recurse(inner)
            elements <- recurse(remaining.drop(1))
          } yield strong(innerElements) :: elements

        case FreeText(PageBreak) =>
          recurse(xs).map(elems => hr(`class` := "pagebreak") :: elems)

        case FreeText(Centered) =>
          recurse(xs)

        case Annotation(content) =>
          if (preview) {
            recurse(xs).map(elems => span(`class` := "openlaw-annotation")(text(content)) :: elems)
          } else {
            recurse(xs)
          }

        case Title(title) =>
          recurse(xs).map(elems => h1(`class` := "signature-title")(title.title) :: elems)

        case x =>
          logger.warn(s"unhandled element: $x")
          recurse(xs)
      }
    }
  }
}
