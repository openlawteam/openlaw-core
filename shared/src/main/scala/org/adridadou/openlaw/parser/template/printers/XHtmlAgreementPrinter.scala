package org.adridadou.openlaw.parser.template.printers

import java.util.concurrent.atomic.AtomicInteger

import cats.implicits._
import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.parser.template.variableTypes.IdentityType
import scalatags.Text.all._
import slogging._

import scala.annotation.tailrec

object XHtmlAgreementPrinter {

  /** Implicit value class to enrich the List[Frag] with a function to print
    * its content as an XHTML string.
    */
  implicit class FragsPrinter(val frags: List[Frag]) extends AnyVal {
    def print: String =
      frags
        .foldLeft(new StringBuilder)({
          case (builder, frag) => builder.append(frag.render)
        })
        .toString
  }
}

final case class XHtmlAgreementPrinter(
    preview: Boolean
) extends LazyLogging {

  private def partitionAtItem[T](seq: List[T], t: T): (List[T], List[T]) =
    partitionAt(seq) { case item if item.equals(t) => true }

  private def partitionAt[T](
      seq: List[T]
  )(pf: PartialFunction[T, Boolean]): (List[T], List[T]) = {
    seq.prefixLength { x =>
      (!pf.isDefinedAt(x)) || !pf(x)
    } match {
      case 0      => (seq, Nil)
      case length => seq.take(length) -> seq.drop(length)
    }
  }

  // separate content for each of the sections at this level
  private def partitionSections(
      level: Int,
      seq: List[AgreementElement]
  ): List[(SectionElement, List[AgreementElement])] = seq match {
    case Nil => Nil
    case (section: SectionElement) :: xs =>
      val (content, remaining) = partitionAt(xs) {
        case SectionElement(_, thisLevel, _, _, _, _) if thisLevel === level =>
          true
      }
      (section -> content) +: partitionSections(level, remaining)
    case first :: _ =>
      throw new RuntimeException(
        s"bug found! the first element should be a SectionElement but was '${first.getClass.getSimpleName}' instead"
      )
  }

  @tailrec private def addBreaks(
      remaining: List[Frag],
      result: List[Frag] = Nil
  ): List[Frag] = remaining match {
    case Nil      => result
    case x :: Nil => result :+ x
    case x :: xs  => addBreaks(xs, result :+ x :+ br())
  }

  private def text(str: String): List[Frag] =
    addBreaks(str.split("\n", -1).map(stringFrag).toList)

  private[this] val paragraphCounter = new AtomicInteger()

  def printRoot(paragraphs: List[Paragraph]): String =
    html(
      body(printParagraphs(paragraphs))
    ).render

  def printParagraphs(paragraphs: List[Paragraph]): List[Frag] =
    printFragments(paragraphs)

  def printFragments(elements: List[AgreementElement]): List[Frag] =
    tailRecurse(elements, 0, false)

  private def generateClass(
      cls: String,
      pair: Option[(Alignment, Border)]
  ): String =
    pair
      .map {
        case (alignment, border) =>
          val alignClass = alignment match {
            case LeftAlignment   => "align-left"
            case CenterAlignment => "align-center"
            case RightAlignment  => "align-right"
          }
          val borderClass = border match {
            case ShowBorder => "border-show"
            case HideBorder => "border-hide"
          }
          s"$cls $alignClass $borderClass"
      }
      .getOrElse(cls)

  private def recurse(
      elements: List[AgreementElement],
      conditionalBlockDepth: Int,
      inSection: Boolean,
      continue: List[Frag] => List[Frag] = identity
  ): List[Frag] =
    tailRecurse(elements, conditionalBlockDepth, inSection, continue)

  @tailrec private def tailRecurse(
      elements: List[AgreementElement],
      conditionalBlockDepth: Int,
      inSection: Boolean,
      continue: List[Frag] => List[Frag] = identity
  ): List[Frag] =
    elements match {
      case Nil =>
        continue(Nil)

      case x :: xs =>
        x match {

          // If this paragraph contains a section definition, extract all following sections for processing together
          case Paragraph((_: SectionElement) :: _) =>
            val fixed = elements map {
              case Paragraph((s: SectionElement) :: xs) =>
                List(s, Paragraph(PlainText(s.value + " ") :: xs))
              case x => List(x)
            }

            tailRecurse(
              fixed.flatten,
              conditionalBlockDepth,
              inSection,
              continue
            )

          case Paragraph(Nil) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, continue)

          case Paragraph(paragraphElements) =>
            val paragraphCount = paragraphCounter.incrementAndGet()

            // See if this paragraph is centered
            val (align, remaining) = paragraphElements match {
              case FreeText(Centered) :: xs   => (List("align-center"), xs)
              case FreeText(Indent) :: xs     => (List("indent"), xs)
              case FreeText(RightAlign) :: xs => (List("align-right"), xs)
              case FreeText(RightThreeQuarters) :: xs =>
                (List("align-right-three-quarters"), xs)
              case seq => (Nil, seq)
            }

            // Setup classes to be added to this paragraph element
            val classes = (if (!inSection) List("no-section") else Nil) ++ align
            val paragraph = if (classes.isEmpty) {
              recurse(remaining, conditionalBlockDepth, inSection, { elems =>
                List(p(elems))
              })
            } else {
              recurse(remaining, conditionalBlockDepth, inSection, { elems =>
                List(p(`class` := classes.mkString(" "))(elems))
              })
            }

            // Recurse on the paragraph contents to render them
            val innerFrag = if (preview) {
              List(
                div(`class` := s"openlaw-paragraph paragraph-$paragraphCount")(
                  paragraph
                )
              )
            } else {
              paragraph
            }

            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(innerFrag ++ elems)
            })

          case t: TableElement =>
            val frag = table(`class` := "markdown-table")(
              tr(`class` := "markdown-table-row")(
                t.header.zipWithIndex.map {
                  case (header: List[AgreementElement], i: Int) =>
                    th(
                      `class` := generateClass(
                        "markdown-table-header",
                        t.alignment.get(i)
                      )
                    )(
                      recurse(header, conditionalBlockDepth, inSection)
                    )
                }
              ),
              t.rows.map { row =>
                tr(`class` := "markdown-table-row")(
                  row.zipWithIndex.map {
                    case (row, i) =>
                      td(
                        `class` := generateClass(
                          "markdown-table-data",
                          t.alignment.get(i)
                        )
                      )(
                        recurse(row, conditionalBlockDepth, inSection)
                      )
                  }
                )
              }
            )
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(frag +: elems)
            })

          case section @ SectionElement(_, level, _, _, _, _) =>
            // partition out all content that will be within the newly defined sections
            val higherLevel = level - 1
            val (content, remaining) = partitionAt(xs) {
              case SectionElement(_, thisLevel, _, _, _, _)
                  if thisLevel === higherLevel =>
                true
            }
            // Partition the elements into sections at this level
            val sections = partitionSections(level, section +: content)
            val afterBreak = sections.foldLeft(List[AgreementElement]()) {
              case (accu, (_, elements)) =>
                // bifurcate the sections around the section break
                val (_, after) =
                  elements.span(_ != Paragraph(List(FreeText(SectionBreak))))
                accu ++ after
            }

            if (afterBreak.nonEmpty) {
              val frag = ul(`class` := s"list-lvl-$level")(
                sections.map {
                  case (_, elements) =>
                    // section break elements and after should not be given the `ul/list` class
                    val filtered = elements filterNot afterBreak.contains
                    recurse(filtered, conditionalBlockDepth, true, { elems =>
                      List(li(elems))
                    })
                }
              )
              tailRecurse(afterBreak, conditionalBlockDepth, inSection, {
                elems =>
                  continue(frag +: elems)
              })
            } else {
              val frag = ul(`class` := s"list-lvl-$level")(
                sections.map { section =>
                  recurse(section._2, conditionalBlockDepth, true, { elems =>
                    List(li(elems))
                  })
                }
              )
              tailRecurse(remaining, conditionalBlockDepth, inSection, {
                elems =>
                  continue(frag +: elems)
              })
            }

          case VariableElement(name, variableType, content, dependencies) =>
            // Do not highlight identity variables
            val highlightType =
              variableType.forall(v => !IdentityType.identityTypes.contains(v))

            // Only add styling to highlight variable if there are no hidden variables that are dependencies for this one
            val frags =
              if (highlightType && preview) {
                val nameClass = name.name.replace(" ", "-")
                List(
                  span(
                    `class` := s"markdown-variable markdown-variable-$nameClass"
                  )(recurse(content, conditionalBlockDepth, inSection))
                )
              } else {
                recurse(content, conditionalBlockDepth, inSection)
              }
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(frags ++ elems)
            })

          case ImageElement(url) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(
                img(`class` := "markdown-embedded-image", src := url) +: elems
              )
            })

          case ConditionalStart(dependencies) =>
            val addDepth =
              if (preview) {
                1
              } else {
                0
              }
            tailRecurse(
              xs,
              conditionalBlockDepth + addDepth,
              inSection,
              continue
            )

          case ConditionalEnd(dependencies) =>
            val removeDepth =
              if (preview) {
                1
              } else {
                0
              }
            tailRecurse(
              xs,
              conditionalBlockDepth - removeDepth,
              inSection,
              continue
            )

          case Link(label, url) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(a(href := url)(label) +: elems)
            })

          case PlainText(str) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(text(str) ++ elems)
            })

          case FreeText(t: Text) =>
            // This might be necessary for some cases, but has not been enabled yet
            // Consume any following text elements which are only newlines
            //val remaining = xs.dropWhile(_ === FreeText(Text("\n")))

            // Generate text output
            val innerFrag = text(t.str)

            val spanFrag: List[Frag] = if (conditionalBlockDepth > 0) {
              List(span(`class` := "markdown-conditional-block")(innerFrag))
            } else {
              innerFrag
            }

            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(spanFrag ++ elems)
            })

          case FreeText(Em) =>
            val (inner, remaining) = partitionAtItem[AgreementElement](xs, x)
            val frag = em(recurse(inner, conditionalBlockDepth, inSection))
            tailRecurse(remaining.drop(1), conditionalBlockDepth, inSection, {
              elems =>
                continue(frag +: elems)
            })

          case FreeText(Strong) =>
            val (inner, remaining) = partitionAtItem(xs, x)
            val frag = strong(recurse(inner, conditionalBlockDepth, inSection))
            tailRecurse(remaining.drop(1), conditionalBlockDepth, inSection, {
              elems =>
                continue(frag +: elems)
            })

          case FreeText(Under) =>
            val (inner, remaining) = partitionAtItem(xs, x)
            val frag = u(recurse(inner, conditionalBlockDepth, inSection))
            tailRecurse(remaining.drop(1), conditionalBlockDepth, inSection, {
              elems =>
                continue(frag +: elems)
            })

          case FreeText(PageBreak) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(hr(`class` := "pagebreak") +: elems)
            })

          case FreeText(SectionBreak) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(hr(`class` := "section-break") +: elems)
            })

          case FreeText(Centered) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, continue)

          case HeaderAnnotation(content) =>
            if (preview) {
              tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
                continue(
                  span(`class` := "openlaw-annotation-header")(text(content)) +: elems
                )
              })
            } else {
              tailRecurse(xs, conditionalBlockDepth, inSection, continue)
            }

          case NoteAnnotation(content) =>
            if (preview) {
              tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
                continue(
                  span(`class` := "openlaw-annotation-note")(text(content)) +: elems
                )
              })

            } else {
              tailRecurse(xs, conditionalBlockDepth, inSection, continue)
            }

          case Title(title) =>
            tailRecurse(xs, conditionalBlockDepth, inSection, { elems =>
              continue(h1(`class` := "signature-title")(title.title) +: elems)
            })

          case x =>
            logger.warn(s"unhandled element: $x")
            tailRecurse(xs, conditionalBlockDepth, inSection, continue)
        }
    }
}
