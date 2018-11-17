package org.scalatra

import scala.util.control.ControlThrowable

sealed trait ActionInterruptionException
class HaltException(val response: ActionResult) extends ControlThrowable with ActionInterruptionException
class PassException extends ControlThrowable with ActionInterruptionException

trait ActionInterruptions {
  self: ResultConverters =>

  protected def halt[T](status: java.lang.Integer = null, body: T = (), headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]): Unit = {
    val result = converter.convert(body)

    throw new HaltException(converter.convert(body).copy(
      status  = if(status == null) result.status else status,
      headers = result.headers ++ headers
    ))
  }

  protected def halt(result: ActionResult): Unit = {
    throw new HaltException(result)
  }

  protected def redirect(path: String): Unit = {
    halt(Found(path))
  }

  protected def pass(): Unit = {
    throw new PassException()
  }

}