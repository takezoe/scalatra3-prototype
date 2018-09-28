package org.scalatra

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.file.Files

import cats.effect.IO
import org.http4s.{Charset, Request}

/**
 * Wrapper for the http4s's Request[IO].
 */
class ScalatraRequest(private[scalatra] val underlying: Request[IO],
  private[scalatra] val pathParams: Map[String, Seq[String]]){

  private val attrs = new scala.collection.mutable.HashMap[String, AnyRef]()

  def setAttribute(key: String, value: AnyRef): Unit = attrs.put(key, value)
  def getAttribute(key: String): AnyRef = attrs.get(key).orNull

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
}
