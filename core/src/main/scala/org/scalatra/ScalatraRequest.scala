package org.scalatra

import java.io.{ByteArrayInputStream, InputStream}

import javax.servlet.http.{HttpServletRequest, HttpSession}
import org.apache.commons.io.IOUtils

import scala.collection.JavaConverters._

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
    val charset = Option(underlying.getContentType).filter(_.contains(";")).flatMap(_.split(";").lastOption).getOrElse("UTF-8")
    new String(cachedBody, charset)
  }

  lazy val queryString: String = underlying.getQueryString

  lazy val contentType: Option[String] = Option(underlying.getContentType)

  lazy val contentLength: Option[Int] = {
    val length = underlying.getContentLength
    if(length < 0) None else Some(length)
  }

  lazy val headers: Map[String, String] = {
    underlying.getHeaderNames.asScala.map { name =>
      name -> underlying.getHeader(name)
    }.toMap
  }

  def inputStream: InputStream = {
    if(cachedBody != null) {
      new ByteArrayInputStream(cachedBody)
    } else {
      underlying.getInputStream
    }
  }

  lazy val cookies: Cookies = {
    val requestCookies: Map[String, String] = underlying.getCookies.map { cookie =>
      cookie.getName -> cookie.getValue
    }.toMap
    new Cookies(requestCookies)
  }

  lazy val sessions: Sessions = new Sessions(underlying)

}

class Cookies(requestCookies: Map[String, String]) {

  private[scalatra] val sweetCookies = scala.collection.mutable.Map[String, String]()

  def get(name: String): Option[String] = sweetCookies.get(name).orElse(requestCookies.get(name))
  def set(name: String, content: String): Unit = sweetCookies.put(name, content)

}

class Sessions(request: HttpServletRequest) {

  private def getSession(): Option[HttpSession] = {
    Option(request.getSession)
  }

  def get(name: String): Option[String] = {
    getSession().flatMap { session =>
      Option(session.getAttribute(name)).map(_.asInstanceOf[String])
    }
  }

  def set(name: String, value: String): Unit = {
    getSession() match {
      case Some(session) => session.setAttribute(name, value)
      case None => throw new ScalatraException("session is not available.")
    }
  }

}

