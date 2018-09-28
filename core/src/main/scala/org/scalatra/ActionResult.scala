package org.scalatra

import cats.effect.IO
import org.http4s._

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

object Ok {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 200, headers = result.headers ++ headers)
  }
}

object Found {
  def apply[T](location: String, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
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
