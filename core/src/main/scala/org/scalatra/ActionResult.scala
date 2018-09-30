package org.scalatra

import cats.effect.IO
import org.http4s._

case class StreamActionResult(
  status: Int,
  body: EntityBody[IO],
  headers: Map[String, String],
  contentType: String = null,
  cookies: Seq[Cookie] = Nil
){
  def toResponse(): Response[IO] = {
    val response = Response[IO](
      status  = Status(status),
      body    = body,
      headers = Headers(
        headers.toList.map { case (name, value) => Header(name, value) } ++
          (if(contentType != null) List(Header("Content-Type", contentType)) else List.empty)
      )
    )
    // Add bitter cookies
    cookies.foldLeft(response){ case (response, cookie) =>
      response.addCookie(cookie)
    }
  }
}

object ActionResult {
  def apply[T](status: Int, body: T, headers: Map[String, String], contentType: String = null)(implicit converter: ResultConverter[T]): StreamActionResult = {
    val result = converter.convert(body)
    if(contentType == null){
      result.copy(status = status, headers = result.headers ++ headers)
    } else {
      result.copy(status = status, contentType = contentType, headers = result.headers ++ headers)
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
