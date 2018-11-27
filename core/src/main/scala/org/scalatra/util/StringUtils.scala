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

}
