package org.scalatra.util

object StringUtils {

  def splitFirst(content: String, delimiter: String): Option[(String, String)] = {
    val i = content.indexOf(delimiter)
    if(i > 0){
      Some((content.substring(0, i), content.substring(i + delimiter.length, content.length)))
    } else {
      None
    }
  }

  def blankOption(str: String): Option[String] = {
    str match {
      case null           => None
      case x if x.isEmpty => None
      case x              => Some(x)
    }
  }

  def isBlank(str: String): Boolean = {
    str match {
      case null           => true
      case x if x.isEmpty => true
      case x              => false
    }
  }

  def nonBlank(str: String): Boolean = {
    !isBlank(str)
  }

}
