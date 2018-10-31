package org.adridadou.openlaw.parser.template.printers

import org.adridadou.openlaw.parser.template._
import org.adridadou.openlaw.values.TemplateTitle

case class PreviewHtmlAgreementPrinter(state:PrinterState = PrinterState(), builder:StringBuilder = StringBuilder.newBuilder) extends HtmlAgreementPrinter {

  override def title(title: TemplateTitle):PreviewHtmlAgreementPrinter = append(s"<h1>${title.title}</h1>")

  override def text(txt: TextElement):PreviewHtmlAgreementPrinter = txt match {
    case Text(t) =>
      append(noHtml(t)
          .replace("\n","<br/>"))
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

  override def link(link: Link): PreviewHtmlAgreementPrinter = append(s"<a href='${link.url}'>${noHtml(link.label)}</a>")

  override def conditionalTextStart():PreviewHtmlAgreementPrinter = append("<span class='markdown-conditional-block'>")

  override def conditionalTextEnd(): PreviewHtmlAgreementPrinter = append("</span>")

  override def variableStart(variableName:String):PreviewHtmlAgreementPrinter = {
    val nameClass = variableName.replace(" ","-")
    append(s"<span class='markdown-variable markdown-variable-$nameClass'>")
  }
  override def variableEnd():PreviewHtmlAgreementPrinter = append("</span>")
  override def sectionStart(section:SectionElement):PreviewHtmlAgreementPrinter =
    this.copy(builder = SectionHelper.handleSections(section.lvl,state.sections, builder), state = state.copy(sections = state.sections.:+(section.lvl)))

  override def sectionEnd(): PreviewHtmlAgreementPrinter = append("</li></ul>")

  override def result:String = SectionHelper.closeSections(state.sections, builder).toString.trim

  override def paragraphHeader(docParagraph:Paragraph):PreviewHtmlAgreementPrinter = append(s"<div class='openlaw-paragraph paragraph-${state.paragraphIndex}'>" + getParagraphHeader(docParagraph)).copy(state = state.copy(headerGenerated = true))

  override def paragraphFooter:PreviewHtmlAgreementPrinter = append("</p></div>").copy(state = state.copy(headerGenerated = false))

  override def sectionHeader(section: SectionElement): PreviewHtmlAgreementPrinter =
    append(section.value + " ")

  override def pageBreak:PreviewHtmlAgreementPrinter = append("<hr/>")
  
  override def table(table: TableElement)(renderCell:(AgreementElement, AgreementPrinter[String]) => Unit):PreviewHtmlAgreementPrinter = {
    append("<table class='markdown-table'>")
    append("<tr class='markdown-table-row'>")
    table.header.foreach { elements =>
      append("<th class='markdown-table-header'>")
      elements.foreach(e => renderCell(e, newState(state.copy(headerGenerated = true))))
      append("</th>")
    }
    append("</tr>")
    table.rows.foreach { row =>
      append("<tr class='markdown-table-row'>")
      row.foreach { elements =>
        append("<td class='markdown-table-data'>")
        elements.foreach(e => renderCell(e, newState(state.copy(headerGenerated = true))))
        append("</td>")
      }
      append("</tr>")
    }
    append("</table>")
    this
  }

  override def newState(state:PrinterState):PreviewHtmlAgreementPrinter = this.copy(state = state)

  private def append(str:String):PreviewHtmlAgreementPrinter = this.copy(builder = builder.append(str))

  private def getParagraphHeader(paragraph: Paragraph): String = {
    val nosection = if (!hasSection(paragraph)) List("no-section") else List()
    val centered = if (isCentered(paragraph)) List("align-center") else List()
    val classes = nosection ++ centered
    if (classes.isEmpty) "<p>" else s"""<p class='${classes.mkString(" ")}'>"""
  }
}
