package org.scalatra

import cats.effect.IO
import org.http4s._

import scala.xml.{Elem, NodeSeq}

case class StreamActionResult(
  status: Int,
  body: EntityBody[IO],
  headers: Map[String, String]
){
  def toResponse(): Response[IO] = Response[IO](
    status  = Status(status),
    body    = body,
    headers = Headers(headers.toList.map { case (name, value) => Header(name, value) })
  )
}

object ActionResult {
  def apply[T](status: Int, body: T, headers: Map[String, String])(implicit converter: ResultConverter[T]): StreamActionResult = {
    val result = converter.convert(body)
    result.copy(status = status, headers = result.headers ++ headers)
  }
}

/**
 * A type class to convert something to ActionResult.
 *
 * @tparam T the result type of action
 */
trait ResultConverter[T]{
  def convert(result: T): StreamActionResult
}

/**
 * Defines implicit instances of ActionResultType type class.
 */
trait ResultConverters {

  implicit object StringResultConverter extends ResultConverter[String] {
    def convert(result: String): StreamActionResult = {
      StreamActionResult(
        status = 200,
        body = fs2.Stream(result.getBytes("UTF-8"): _*),
        headers = Map("Content-Type" -> "text/plain; charset=UTF-8")
      )
    }
  }

  implicit object UnitResultConverter extends ResultConverter[Unit] {
    def convert(result: Unit): StreamActionResult = {
      StreamActionResult(
        status = 200,
        body = EmptyBody,
        headers = Map.empty
      )
    }
  }

  implicit object ByteArrayResultConverter extends ResultConverter[Array[Byte]] {
    def convert(result: Array[Byte]): StreamActionResult = {
      StreamActionResult(
        status = 200,
        body = fs2.Stream(result: _*),
        headers = Map("Content-Type" -> "application/octet-stream")
      )
    }
  }

  implicit object ActionResultResultConverter extends ResultConverter[StreamActionResult] {
    def convert(result: StreamActionResult): StreamActionResult = {
      result
    }
  }

  implicit object ElemResultConverter extends ResultConverter[Elem] {
    def convert(result: Elem): StreamActionResult = {
      StreamActionResult(
        status = 200,
        body = fs2.Stream(result.toString().getBytes("UTF-8"): _*),
        headers = Map("Content-Type" -> "text/html; charset=UTF-8")
      )
    }
  }
}

object Ok {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 200, headers = result.headers ++ headers)
  }
}

object Found {
  def apply[T](location: String, headers: Map[String, String] = Map.empty) = {
    val result = UnitResultConverter.convert(())
    result.copy(status = 302, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object NotFound {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 404, headers = result.headers ++ headers)
  }
}

object BadRequest {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 400, headers = result.headers ++ headers)
  }
}

object InternalServerError {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 500, headers = result.headers ++ headers)
  }
}
