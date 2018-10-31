package org.adridadou.openlaw.parser.template.printers

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.values.TemplateTitle

case class ReviewHtmlAgreementPrinter(state:PrinterState = PrinterState(), builder:StringBuilder = StringBuilder.newBuilder) extends HtmlAgreementPrinter {

  override def title(title: TemplateTitle):ReviewHtmlAgreementPrinter = append(s"<h1 class='signature-title'>${title.title}</h1>")

  override def link(link: Link): ReviewHtmlAgreementPrinter = append(s"<a href='${link.url}'>${noHtml(link.label)}</a>")

  override def text(txt: TextElement):ReviewHtmlAgreementPrinter = txt match {
    case Text(t) =>
      append(noHtml(t).replace("\n","<br/>"))
    case Em if !state.em =>
      append("<em>").newState(state.copy(em = true))
    case Em if state.em =>
      append("</em>").newState(state.copy(em = false))
    case Strong if !state.strong =>
      append("<strong>").newState(state.copy(strong = true))
    case Strong if state.strong =>
      append("</strong>").newState(state.copy(strong = false))
    case PageBreak =>
      pageBreak
    case _ =>
      this
  }

  override def conditionalTextStart():ReviewHtmlAgreementPrinter = this
  override def conditionalTextEnd():ReviewHtmlAgreementPrinter = this

  override def variableStart(variableName:String):ReviewHtmlAgreementPrinter = this
  override def variableEnd():ReviewHtmlAgreementPrinter = this

  override def sectionStart(section:SectionElement):ReviewHtmlAgreementPrinter =
    this.copy(builder = SectionHelper.handleSections(section.lvl,state.sections, builder), state = state.copy(sections = state.sections.:+(section.lvl)))

  override def sectionEnd(): ReviewHtmlAgreementPrinter = append("</li></ul>")

  override def newState(state:PrinterState):ReviewHtmlAgreementPrinter = this.copy(state = state)

  /** Note that this method is side-effecting; it will re-close the section on each call.  It should be called only once
    * and the result reused if needed.
    */
  override def result:String = SectionHelper.closeSections(state.sections, builder).toString.trim

  override def paragraphHeader(docParagraph:Paragraph):ReviewHtmlAgreementPrinter =
    newState(state.copy(headerGenerated = true)).append(getParagraphHeader(docParagraph))

  private def getParagraphHeader(paragraph: Paragraph): String = {
    val noSection = if (!hasSection(paragraph)) List("no-section") else List()
    val centered = if (isCentered(paragraph)) List("align-center") else List()
    val classes = noSection ++ centered
    if (classes.isEmpty) "<p>" else s"""<p class='${classes.mkString(" ")}'>"""
  }

  override def paragraphFooter:ReviewHtmlAgreementPrinter =
    newState(state.copy(headerGenerated = false)).append("</p>")

  override def sectionHeader(section: SectionElement): ReviewHtmlAgreementPrinter =
    append(section.value + " ")

  override def pageBreak:ReviewHtmlAgreementPrinter = append("<hr/>")

  override def table(table: TableElement)(renderCell:(AgreementElement, AgreementPrinter[String]) => Unit):ReviewHtmlAgreementPrinter = {
    append("<table class='markdown-table'>")
    append("<tr class='markdown-table-row'>")
    table.header.foreach { elements =>
      append("<th class='markdown-table-header'>")
      // passes a new printer state with headerGenerated set to true in order to suppress the creation of an extraneous opening paragraph tag
      elements.foreach(e => renderCell(e, newState(state.copy(headerGenerated = true))))
      append("</th>")
    }
    append("</tr>")
    table.rows.foreach { row =>
      append("<tr class='markdown-table-row'>")
      row.foreach { elements =>
        append("<td class='markdown-table-data'>")
        // passes a new printer state with headerGenerated set to true in order to suppress the creation of an extraneous opening paragraph tag
        elements.foreach(e => renderCell(e, newState(state.copy(headerGenerated = true))))
        append("</td>")
      }
      append("</tr>")
    }
    append("</table>")
    this
  }

  private def append(str:String):ReviewHtmlAgreementPrinter =
    this.copy(builder = builder.append(str))
}
