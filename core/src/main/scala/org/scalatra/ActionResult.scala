package org.scalatra

import cats.effect.IO
import org.http4s._

case class ActionResult(
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

trait ActionResultType[T]{
  def toActionResult(result: T): ActionResult
}

object StringActionResultType extends ActionResultType[String] {
  def toActionResult(result: String): ActionResult = {
    ActionResult(
      status = 200,
      body = fs2.Stream(result.getBytes("UTF-8"): _*),
      headers = Map("Content-Type" -> "text/plain; charset=UTF-8")
    )
  }
}
