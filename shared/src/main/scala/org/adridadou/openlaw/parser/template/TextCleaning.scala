package org.adridadou.openlaw.parser.template

object TextCleaning {
  def dots(t: String): String = t.replaceAll("(\\ )+\\.(?!\\.)", ".")
}