package org.scalatra

import java.io.{ByteArrayInputStream, InputStream}
import java.net.URLDecoder
import java.nio.file.Files

import cats.effect.IO
import org.http4s._
import org.http4s.util.CaseInsensitiveString

/**
 * Wrapper for the http4s's Request[IO].
 */
class ScalatraRequest(private[scalatra] val underlying: Request[IO]){

  private val attrs = new scala.collection.mutable.HashMap[String, AnyRef]()

  lazy val formParams: Map[String, Seq[String]] = {
    try {
      underlying.headers.get(CaseInsensitiveString("Content-Type")).collect { case x if x.value == "application/x-www-form-urlencoded" =>
        body.split("&").map { kv =>
          val array = kv.split("=")
          val key   = URLDecoder.decode(array(0), "UTF-8")
          val value = URLDecoder.decode(array(1), "UTF-8")
          (key -> value)
        }.groupBy { case (key, value) =>
          key
        }.map { case (key, kv) =>
          key -> kv.map(_._2).toSeq
        }
      }.getOrElse(Map.empty)
    } catch {
      case _: Exception => Map.empty // TODO log?
    }
  }

  def set(key: String, value: AnyRef): Unit = attrs.put(key, value)
  def get(key: String): Option[AnyRef] = attrs.get(key)
  def contains(key: String): Boolean = attrs.contains(key)

  private var cachedBody: Array[Byte] = null

  private def createBodyCache(): Unit = {
    if(cachedBody == null) {
      val bytes = underlying.body.compile.fold(List.empty[Byte]) { case (acc, byte) => acc :+ byte }
      cachedBody = bytes.unsafeRunSync().toArray
    }
  }

  lazy val body: String = {
    createBodyCache()
    val charset = underlying.contentType.flatMap(_.charset).getOrElse(Charset.`UTF-8`)
    new String(cachedBody, charset.nioCharset)
  }

  lazy val queryString: String = underlying.queryString
  lazy val contentType: Option[String] = underlying.contentType.map(_.value)
  lazy val contentLength: Option[Long] = underlying.contentLength
  lazy val headers: Map[String, String] = underlying.headers.map { x => x.name.toString() -> x.value }.toMap

  def inputStream: InputStream = {
    if(cachedBody != null) {
      new ByteArrayInputStream(cachedBody)
    } else {
      underlying.contentLength match {
        case Some(length) if length < 1024 * 1024 * 1 =>
          createBodyCache()
          new ByteArrayInputStream(cachedBody)
        case _ =>
          val tempFile = Files.createTempFile("scalatra-", ".req")
          underlying.body.through(fs2.io.file.writeAll(tempFile)).compile.drain.unsafeRunSync()
          Files.newInputStream(tempFile) // TODO Delete temp file?
      }
    }
  }

  lazy val cookies: Cookies = {
    val requestCookies: Map[String, String] = {
      val cookies = org.http4s.headers.Cookie.from(underlying.headers)
      cookies.map { cookies =>
        cookies.values.toList.map { cookie =>
          cookie.name -> cookie.content
        }.toMap
      }.getOrElse(Map.empty)
    }
    new Cookies(requestCookies)
  }

}

class Cookies(requestCookies: Map[String, String]) {

  private[scalatra] val sweetCookies = scala.collection.mutable.Map[String, String]()

  def get(name: String): Option[String] = sweetCookies.get(name).orElse(requestCookies.get(name))
  def update(name: String, content: String): Unit = sweetCookies.update(name, content)
  def put(name: String, content: String): Unit = sweetCookies.put(name, content)

}
