package org.scalatra

import cats.effect.IO
import org.http4s.Response

import scala.util.control.ControlThrowable

sealed trait ActionInterruptionException
class HaltException(val response: Response[IO]) extends ControlThrowable with ActionInterruptionException
class PassException extends ControlThrowable with ActionInterruptionException

trait ActionInterruptions {
  self: ResultConverters =>

  protected def halt[T](status: java.lang.Integer = null, body: T = (), headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]): Unit = {
    val result = converter.convert(body)
    val response = result.copy(
      status  = if(status == null) result.status else status,
      headers = result.headers ++ headers
    ).toResponse()

    throw new HaltException(response)
  }

  protected def halt(result: StreamActionResult): Unit = {
    throw new HaltException(result.toResponse())
  }

  protected def redirect(path: String): Unit = {
    halt(Found(path))
  }

  protected def pass(): Unit = {
    throw new PassException()
  }

}