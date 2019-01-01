package org.scalatra

import java.io.{InputStream, OutputStream}

import org.apache.commons.io.IOUtils

import scala.util.Try

/**
 * The results of Action returned from Action#run().
 * Scalatra renders the response using information stored in this object.
 */
case class ActionResult(
  status: Int,
  body: Body,
  headers: Map[String, String],
  contentType: String
)

sealed trait Body {
  def writeTo(out: OutputStream): Unit
}

case class ByteArrayBody(bytes: Array[Byte]) extends Body {
  def writeTo(out: OutputStream): Unit = out.write(bytes)
}

case class InputStreamBody(in: InputStream) extends Body {
  override def writeTo(out: OutputStream): Unit = {
    try {
      IOUtils.copy(in, out)
    } finally {
      Try { in.close() }
    }
  }
}

object Ok {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 200, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Created {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 201, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Accepted {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 202, contentType = contentType, headers = result.headers ++ headers)
  }
}

object NonAuthoritativeInformation {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 203, contentType = contentType, headers = result.headers ++ headers)
  }
}

object NoContent {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 204, contentType = contentType, headers = result.headers ++ headers)
  }
}

object ResetContent {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 205, contentType = contentType, headers = result.headers ++ headers)
  }
}

object PartialContent {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 206, contentType = contentType, headers = result.headers ++ headers)
  }
}

object MultiStatus {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 207, contentType = contentType, headers = result.headers ++ headers)
  }
}

object AlreadyReported {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 208, contentType = contentType, headers = result.headers ++ headers)
  }
}

object IMUsed {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 226, contentType = contentType, headers = result.headers ++ headers)
  }
}

object MultipleChoices {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 300, contentType = contentType, headers = result.headers ++ headers)
  }
}

object MovedPermanently {
  def apply[T](location: String, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
    result.copy(status = 301, contentType = contentType, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object Found {
  def apply[T](location: String, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
    result.copy(status = 302, contentType = contentType, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object SeeOther {
  def apply[T](location: String, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
    result.copy(status = 303, contentType = contentType, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object NotModified {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 304, contentType = contentType, headers = result.headers ++ headers)
  }
}

object UseProxy {
  def apply[T](location: String, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
    result.copy(status = 305, contentType = contentType, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object TemporaryRedirect {
  def apply[T](location: String, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
    result.copy(status = 307, contentType = contentType, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object PermanentRedirect {
  def apply[T](location: String, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[Unit]) = {
    val result = converter.convert(())
    result.copy(status = 308, contentType = contentType, headers = result.headers ++ headers ++ Map("Location" -> location))
  }
}

object BadRequest {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 400, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Unauthorized {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 401, contentType = contentType, headers = result.headers ++ headers)
  }
}

object PaymentRequired {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 402, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Forbidden {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 403, contentType = contentType, headers = result.headers ++ headers)
  }
}

object NotFound {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 404, contentType = contentType, headers = result.headers ++ headers)
  }
}

object MethodNotAllowed {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 405, contentType = contentType, headers = result.headers ++ headers)
  }
}

object NotAcceptable {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 406, contentType = contentType, headers = result.headers ++ headers)
  }
}

object ProxyAuthenticationRequired {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 407, contentType = contentType, headers = result.headers ++ headers)
  }
}

object RequestTimeout {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 408, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Conflict {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 409, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Gone {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 410, contentType = contentType, headers = result.headers ++ headers)
  }
}

object LengthRequired {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 411, contentType = contentType, headers = result.headers ++ headers)
  }
}

object PreconditionFailed {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 412, contentType = contentType, headers = result.headers ++ headers)
  }
}

object RequestEntityTooLarge {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 413, contentType = contentType, headers = result.headers ++ headers)
  }
}

object RequestURITooLong {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 414, contentType = contentType, headers = result.headers ++ headers)
  }
}

object UnsupportedMediaType {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 415, contentType = contentType, headers = result.headers ++ headers)
  }
}

object RequestedRangeNotSatisfiable {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 416, contentType = contentType, headers = result.headers ++ headers)
  }
}

object ExpectationFailed {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 417, contentType = contentType, headers = result.headers ++ headers)
  }
}

object UnprocessableEntity {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 422, contentType = contentType, headers = result.headers ++ headers)
  }
}

object Locked {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 423, contentType = contentType, headers = result.headers ++ headers)
  }
}

object FailedDependency {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 424, contentType = contentType, headers = result.headers ++ headers)
  }
}

object UpgradeRequired {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 426, contentType = contentType, headers = result.headers ++ headers)
  }
}

object PreconditionRequired {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 428, contentType = contentType, headers = result.headers ++ headers)
  }
}

object TooManyRequests {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 429, contentType = contentType, headers = result.headers ++ headers)
  }
}

object RequestHeaderFieldsTooLarge {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 431, contentType = contentType, headers = result.headers ++ headers)
  }
}

object InternalServerError {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 500, contentType = contentType, headers = result.headers ++ headers)
  }
}

object NotImplemented {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 501, contentType = contentType, headers = result.headers ++ headers)
  }
}

object BadGateway {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 502, contentType = contentType, headers = result.headers ++ headers)
  }
}

object ServiceUnavailable {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 503, contentType = contentType, headers = result.headers ++ headers)
  }
}

object GatewayTimeout {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 504, contentType = contentType, headers = result.headers ++ headers)
  }
}

object HTTPVersionNotSupported {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 505, contentType = contentType, headers = result.headers ++ headers)
  }
}

object VariantAlsoNegotiates {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 506, contentType = contentType, headers = result.headers ++ headers)
  }
}

object InsufficientStorage {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 507, contentType = contentType, headers = result.headers ++ headers)
  }
}

object LoopDetected {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 508, contentType = contentType, headers = result.headers ++ headers)
  }
}

object NotExtended {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 510, contentType = contentType, headers = result.headers ++ headers)
  }
}

object NetworkAuthenticationRequired {
  def apply[T](body: T = (): Unit, contentType: String = null, headers: Map[String, String] = Map.empty)(implicit converter: ResultConverter[T]) = {
    val result = converter.convert(body)
    result.copy(status = 511, contentType = contentType, headers = result.headers ++ headers)
  }
}
