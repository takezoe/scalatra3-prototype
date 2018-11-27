package org.scalatra

import java.io.{ByteArrayInputStream, InputStream}

import javax.servlet.http.{HttpServletRequest, HttpSession}
import org.apache.commons.io.IOUtils
import org.scalatra.util.StringUtils

import scala.collection.JavaConverters._
import scala.collection.immutable.TreeMap

/**
 * A wrapper of HttpServletRequest
 *
 * @param underlying underlying HttpServletRequest instance
 */
class ScalatraRequest(private[scalatra] val underlying: HttpServletRequest){

  def set(key: String, value: AnyRef): Unit = underlying.setAttribute(key, value)
  def get(key: String): Option[AnyRef] = Option(underlying.getAttribute(key))
  def contains(key: String): Boolean = underlying.getAttribute(key) != null

  private var cachedBody: Array[Byte] = null

  private def createBodyCache(): Unit = {
    if(cachedBody == null) {
      cachedBody = IOUtils.toByteArray(underlying.getInputStream)
    }
  }

  lazy val body: String = {
    createBodyCache()
    val charset = StringUtils.splitFirst(underlying.getContentType, ";").map(_._2).getOrElse("UTF-8")
    new String(cachedBody, charset)
  }

  lazy val queryString: String = underlying.getQueryString

  lazy val contentType: Option[String] = Option(underlying.getContentType)

  lazy val contentLength: Option[Int] = {
    val length = underlying.getContentLength
    if(length < 0) None else Some(length)
  }

  lazy val headers: Map[String, String] = {
    TreeMap[String, String](
      underlying.getHeaderNames.asScala.map { name =>
        name -> underlying.getHeader(name)
      }.toSeq: _*
    )(Ordering.by(_.toLowerCase))
  }

  lazy val requestMethod: HttpMethod = HttpMethod(underlying.getMethod)

  def inputStream: InputStream = {
    if(cachedBody != null) {
      new ByteArrayInputStream(cachedBody)
    } else {
      underlying.getInputStream
    }
  }

  lazy val cookies: Cookies = {
    val requestCookies: Map[String, String] = Option(underlying.getCookies).map{
      _.map { cookie =>
        cookie.getName -> cookie.getValue
      }.toMap
    }.getOrElse(Map.empty)
    new Cookies(requestCookies)
  }

}

class Cookies(requestCookies: Map[String, String]) {

  private[scalatra] val sweetCookies = scala.collection.mutable.Map[String, String]()

  def get(name: String): Option[String] = sweetCookies.get(name).orElse(requestCookies.get(name))
  def set(name: String, content: String): Unit = sweetCookies.put(name, content)

}
