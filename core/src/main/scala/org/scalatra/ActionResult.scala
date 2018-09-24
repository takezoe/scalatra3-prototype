package org.scalatra

import cats.effect.IO
import org.http4s.{Response, Status}

case class ActionResult(status: Int){
  def toResponse(): Response[IO] = Response[IO](status = Status(status))
}

trait ActionResultType[T]{
  def toActionResult(result: T): ActionResult
}

object StringActionResultType extends ActionResultType[String]{
  def toActionResult(result: String): ActionResult = ActionResult(200)
}
