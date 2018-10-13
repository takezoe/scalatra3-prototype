package org.scalatra

import java.io.{File, FileInputStream, InputStream}

import cats.effect.IO
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
        contentType = "text/plain; charset=UTF-8",
        headers = Map.empty
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
        contentType = "application/octet-stream",
        headers = Map.empty
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
        contentType = "text/html; charset=UTF-8",
        headers = Map.empty
      )
    }
  }

  implicit object InputStreamResultConverter extends ResultConverter[InputStream] {
    def convert(result: InputStream): StreamActionResult = {
      StreamActionResult(
        status = 200,
        body = fs2.io.readInputStream(IO.pure(result), 1024 * 8),
        contentType = "application/octet-stream",
        headers = Map.empty
      )
    }
  }

  implicit object FileResultConverter extends ResultConverter[File] {
    def convert(result: File): StreamActionResult = {
      StreamActionResult(
        status = 200,
        body = fs2.io.readInputStream(IO.pure(new FileInputStream(result): InputStream), 1024 * 8),
        contentType = "application/octet-stream", // TODO MIME type should be decided by filename
        headers = Map.empty // TODO Content-Disposition should be set?
      )
    }
  }
}