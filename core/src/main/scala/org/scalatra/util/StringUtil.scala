package org.scalatra.util

import java.net.URLDecoder

object StringUtil {

  def splitFirst(content: String, delimiter: String): Option[(String, String)] = {
    val i = content.indexOf(delimiter)
    if(i > 0){
      Some((content.substring(0, i), content.substring(i + delimiter.length, content.length)))
    } else {
      None
    }
  }

  // TODO Use Scalatra's UrlCodingUtils?
  def urlDecode(value: String): String = {
    URLDecoder.decode(value.replace('+', ' '), "UTF-8")
  }

}
