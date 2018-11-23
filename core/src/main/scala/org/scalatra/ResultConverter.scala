package org.scalatra

import java.io.{File, FileInputStream, InputStream}

import scala.xml.Elem

/**
 * A type class to convert something to ActionResult.
 *
 * @tparam T the result type of action
 */
trait ResultConverter[T]{
  def convert(result: T): ActionResult
}

/**
 * Defines implicit instances of ActionResultType type class.
 */
trait ResultConverters {

  implicit object ActionResultResultConverter extends ResultConverter[ActionResult] {
    def convert(result: ActionResult): ActionResult = {
      result
    }
  }

  implicit object StringResultConverter extends ResultConverter[String] {
    def convert(result: String): ActionResult = {
      new ActionResult(
        status = 200,
        body = ByteArrayBody(result.getBytes("UTF-8")),
        contentType = "text/plain; charset=UTF-8",
        headers = Map.empty
      )
    }
  }

  implicit object UnitResultConverter extends ResultConverter[Unit] {
    def convert(result: Unit): ActionResult = {
      ActionResult(
        status = 200,
        body = ByteArrayBody(Array.empty),
        contentType = null,
        headers = Map.empty,
      )
    }
  }

  implicit object ByteArrayResultConverter extends ResultConverter[Array[Byte]] {
    def convert(result: Array[Byte]): ActionResult = {
      ActionResult(
        status = 200,
        body = ByteArrayBody(result),
        contentType = "application/octet-stream",
        headers = Map.empty
      )
    }
  }

  implicit object ElemResultConverter extends ResultConverter[Elem] {
    def convert(result: Elem): ActionResult = {
      ActionResult(
        status = 200,
        body = ByteArrayBody(result.toString().getBytes("UTF-8")),
        contentType = "text/html; charset=UTF-8",
        headers = Map.empty
      )
    }
  }

  implicit object InputStreamResultConverter extends ResultConverter[InputStream] {
    def convert(result: InputStream): ActionResult = {
      ActionResult(
        status = 200,
        body = InputStreamBody(result),
        contentType = "application/octet-stream",
        headers = Map.empty
      )
    }
  }

  implicit object FileResultConverter extends ResultConverter[File] {
    def convert(result: File): ActionResult = {
      ActionResult(
        status = 200,
        body = InputStreamBody(new FileInputStream(result)),
        contentType = "application/octet-stream", // TODO MIME type should be decided by filename
        headers = Map.empty // TODO Content-Disposition should be set?
      )
    }
  }
}