package org.adridadou.openlaw.parser.template.printers

trait HtmlAgreementPrinter extends AgreementPrinter[String] {

  def noHtml(text:String):String = text
    .replace(">","&gt;")
    .replace("<", "&lt;")

}
