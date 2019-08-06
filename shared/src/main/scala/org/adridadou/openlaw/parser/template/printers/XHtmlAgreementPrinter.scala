
package org.adridadou.openlaw.parser.template.printers

import java.util.concurrent.atomic.AtomicInteger

import cats.implicits._
import org.adridadou.openlaw.parser.contract.ParagraphEdits
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.IdentityType
import scalatags.Text.all._
import slogging._

import scala.annotation.tailrec

object XHtmlAgreementPrinter {

  /** Implicit value class to enrich the Seq[Frag] with a function to print
    * its content as an XHTML string.
    */
  implicit class FragsPrinter(val frags: Seq[Frag]) extends AnyVal {
    def print: String = frags
      .foldLeft(new StringBuilder)({
        case (builder, frag) => builder.append(frag.render)
      }).toString
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

  def printRoot(paragraphs: Seq[Paragraph]): String = html(
      body(printParagraphs(paragraphs))
    ).render


  def printParagraphs(paragraphs: Seq[Paragraph]): Seq[Frag] = {
    printFragments(paragraphs)
  }

  def printFragments(elements: Seq[AgreementElement]): Seq[Frag] =
    tailRecurse(elements, 0, false)

  private final def recurse(elements: Seq[AgreementElement], conditionalBlockDepth: Int, inSection: Boolean, continue: Seq[Frag] => Seq[Frag] = identity): Seq[Frag] = {
    tailRecurse(elements, conditionalBlockDepth, inSection, continue)
  }

  @tailrec private final def tailRecurse(elements: Seq[AgreementElement], conditionalBlockDepth: Int, inSection: Boolean, continue: Seq[Frag] => Seq[Frag] = identity): Seq[Frag] = {

    elements match {
      case Seq() =>
        continue(Seq())

      case Seq(x, xs @ _*) => x match {

        // If this paragraph contains a section definition, extract all following sections for processing together
        case p @ Paragraph(Seq(s: SectionElement, _*)) =>

          val fixed = elements map {
            case Paragraph(Seq(s: SectionElement, xs @ _*)) => Seq(s, Paragraph(PlainText(s.value + " ") :: xs.toList))
            case x => Seq(x)
          }

          tailRecurse(fixed.flatten, conditionalBlockDepth, inSection, continue)

        case Paragraph(Seq()) =>
          tailRecurse(xs, conditionalBlockDepth, inSection, continue)

        case Paragraph(paragraphElements) =>
          val paragraphCount = paragraphCounter.incrementAndGet()

          // Generate overridden paragraph contents if required
          val overridden = paragraphEdits.edits.get(paragraphCount - 1) match {

            // If there is an overridden paragraph, render its content instead of this paragraph
            case Some(str) =>
              // TODO: This should be refactored to use Result, but this is deferred since this printer is very sensitive to stack overflows
              MarkdownParser.parseMarkdownOrThrow(str).map(FreeText)

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
            recurse(remaining, conditionalBlockDepth, inSection, { elems => Seq(p(elems)) })
          } else {
            recurse(remaining, conditionalBlockDepth, inSection, { elems => Seq(p(`class` := classes.mkString(" "))(elems)) })
          }

          // Recurse on the paragraph contents to render them
          val innerFrag = if (preview) {
            Seq(div(`class` := s"openlaw-paragraph paragraph-$paragraphCount")(paragraph))
          } else {
            paragraph
          }

          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(innerFrag ++ elems) })

        case t: TableElement =>
          val frag = table(`class` := "markdown-table")(
            tr(`class` := "markdown-table-row")(
              t.header.map { t =>
                th(`class` := "markdown-table-header")(recurse(t, conditionalBlockDepth, inSection))
              }
            ),
            t.rows.map { row =>
              tr(`class` := "markdown-table-row")(
                row.map { t =>
                  td(`class` := "markdown-table-data")(recurse(t, conditionalBlockDepth, inSection))
                }
              )
            }
          )
          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(frag +: elems) })

        case section@SectionElement(_, level, _, _, _, _) =>
          // partition out all content that will be within the newly defined sections
          val higherLevel = level - 1
          val (content, remaining) = partitionAt(xs) { case SectionElement(_, thisLevel, _, _, _, _) if thisLevel === higherLevel => true }

          // Partition the elements into sections at this level
          val sections = partitionSections(level, section +: content)
          val frag = ul(`class` := s"list-lvl-$level")(
            sections.map { section =>
              recurse(section._2, conditionalBlockDepth, true, { elems => Seq(li(elems)) })
            }
          )
          tailRecurse(remaining, conditionalBlockDepth, inSection, { elems => continue(frag +: elems) } )


        case VariableElement(name, variableType, content, dependencies) =>
          // Do not highlight identity variables
          val highlightType = variableType.forall(v => !IdentityType.identityTypes.contains(v))

          // Only add styling to highlight variable if there are no hidden variables that are dependencies for this one
          val frags = if (highlightType && preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) {
            val nameClass = name.name.replace(" ", "-")
            Seq(span(`class` := s"markdown-variable markdown-variable-$nameClass")(recurse(content, conditionalBlockDepth, inSection)))
          } else {
            recurse(content, conditionalBlockDepth, inSection)
          }
          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(frags ++ elems) })

        case ImageElement(url) =>
          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(img(`class` := "markdown-embedded-image", src := url) +: elems) })

        case ConditionalStart(dependencies) =>
          val addDepth = if (preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) 1 else 0
          tailRecurse(xs, conditionalBlockDepth + addDepth, inSection, continue)

        case ConditionalEnd(dependencies) =>
          val removeDepth = if (preview && dependencies.forall(variable => !hiddenVariables.contains(variable))) 1 else 0
          tailRecurse(xs, conditionalBlockDepth - removeDepth, inSection, continue)

        case Link(label, url) =>
          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(a(href := url)(label) +: elems) } )

        case PlainText(str) =>
          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(text(str) ++ elems) })

        case FreeText(t: Text) =>

          // This might be necessary for some cases, but has not been enabled yet
          // Consume any following text elements which are only newlines
          //val remaining = xs.dropWhile(_ === FreeText(Text("\n")))

          // Generate text output
          val innerFrag = text(t.str)

          val spanFrag: Seq[Frag] = if (conditionalBlockDepth > 0) {
            Seq(span(`class` := "markdown-conditional-block")(innerFrag))
          } else {
            innerFrag
          }

          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(spanFrag ++ elems) } )

        case FreeText(Em) =>
          val (inner, remaining) = partitionAtItem[AgreementElement](xs, x)
          val frag = em(recurse(inner, conditionalBlockDepth, inSection))
          tailRecurse(remaining.drop(1), conditionalBlockDepth, inSection, { elems => continue(frag +: elems) })

        case FreeText(Strong) =>
          val (inner, remaining) = partitionAtItem(xs, x)
          val frag = strong(recurse(inner, conditionalBlockDepth, inSection))
          tailRecurse(remaining.drop(1), conditionalBlockDepth, inSection, { elems => continue(frag +: elems) })

        case FreeText(Under) =>
          val (inner, remaining) = partitionAtItem(xs, x)
          val frag = u(recurse(inner, conditionalBlockDepth, inSection))
          tailRecurse(remaining.drop(1), conditionalBlockDepth, inSection, { elems => continue(frag +: elems) })

        case FreeText(PageBreak) =>
          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(hr(`class` := "pagebreak") +: elems) })

        case FreeText(Centered) =>
          tailRecurse(xs, conditionalBlockDepth, inSection, continue)

        case HeaderAnnotation(content) =>
          if(preview) {
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(span(`class` := "openlaw-annotation-header")(text(content)) +: elems) })
          } else {
            tailRecurse(xs, conditionalBlockDepth, inSection, continue)
          }

        case NoteAnnotation(content) =>
          if(preview) {
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(span(`class` := "openlaw-annotation-note")(text(content)) +: elems) })

          } else {
            tailRecurse(xs, conditionalBlockDepth, inSection, continue)
          }

        case Title(title) =>
          tailRecurse(xs, conditionalBlockDepth, inSection, { elems => continue(h1(`class` := "signature-title")(title.title) +: elems) })

        case x =>
          logger.warn(s"unhandled element: $x")
          tailRecurse(xs, conditionalBlockDepth, inSection, continue)
      }
    }
  }
}
