package org.scalatra

class ScalatraException(message: String)
  extends Exception(message)

import scala.util.control.ControlThrowable

sealed trait ActionInterruptionException
class HaltException(val result: ActionResult) extends ControlThrowable with ActionInterruptionException
class PassException extends ControlThrowable with ActionInterruptionException
