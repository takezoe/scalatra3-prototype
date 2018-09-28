package org.scalatra

import org.http4s.EmptyBody

import scala.xml.Elem

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