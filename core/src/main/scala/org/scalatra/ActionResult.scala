package org.scalatra

import java.io.{InputStream, OutputStream}

import org.apache.commons.io.IOUtils

import scala.util.Try

/**
 * The results of Action returned from Action#run().
 * Scalatra renders the response using information stored in this object.
 */
case class ActionResult(
  status: Int,
  body: Body,
  headers: Map[String, String],
  contentType: String
)

sealed trait Body {
  def writeTo(out: OutputStream): Unit
}

case class ByteArrayBody(bytes: Array[Byte]) extends Body {
  def writeTo(out: OutputStream): Unit = out.write(bytes)
}

case class InputStreamBody(in: InputStream) extends Body {
  override def writeTo(out: OutputStream): Unit = {
    try {
      IOUtils.copy(in, out)
    } finally {
      Try { in.close() }
    }
  }
}

object Ok {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 200, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Found {
  def apply[T](location: String, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
    result.copy(status = 302, contentType = contentType, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object NotFound {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 404, contentType = contentType, headers = result.headers ++ headers)
  }
}

object BadRequest {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 400, contentType = contentType, headers = result.headers ++ headers)
  }
}

object InternalServerError {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 500, contentType = contentType, headers = result.headers ++ headers)
  }
}
