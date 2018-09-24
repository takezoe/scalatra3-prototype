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

object ActionResult {
  def apply[T](status: Int, body: T, headers: Map[String, String])(implicit resultType: ActionResultType[T]): ActionResult = {
    val result = resultType.toActionResult(body)
    result.copy(status = status, headers = result.headers ++ headers)
  }
}

trait ActionResultType[T]{
  def toActionResult(result: T): ActionResult
}

trait ActionResultTypes {
  implicit protected val stringResultType = StringActionResultType
  implicit protected val unitResultType = UnitActionResultType
  implicit protected val actionResultResultType = ActionResultActionResultType
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

object UnitActionResultType extends ActionResultType[Unit] {
  def toActionResult(result: Unit): ActionResult = {
    ActionResult(
      status = 200,
      body = EmptyBody,
      headers = Map.empty
    )
  }
}

object ActionResultActionResultType extends ActionResultType[ActionResult] {
  def toActionResult(result: ActionResult): ActionResult = {
    result
  }
}

object Ok {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit resultType: ActionResultType[T]) = {
    val result = resultType.toActionResult(body)
    result.copy(status = 200, headers = result.headers ++ headers)
  }
}

object NotFound {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit resultType: ActionResultType[T]) = {
    val result = resultType.toActionResult(body)
    result.copy(status = 404, headers = result.headers ++ headers)
  }
}

object BadRequest {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit resultType: ActionResultType[T]) = {
    val result = resultType.toActionResult(body)
    result.copy(status = 400, headers = result.headers ++ headers)
  }
}

object InternalServerError {
  def apply[T](body: T = (): Unit, headers: Map[String, String] = Map.empty)(implicit resultType: ActionResultType[T]) = {
    val result = resultType.toActionResult(body)
    result.copy(status = 500, headers = result.headers ++ headers)
  }
}
