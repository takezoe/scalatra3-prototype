package org.scalatra

import java.io._
import java.nio.file.Files

import cats.effect.IO
import org.http4s._

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable
import scala.util.control.ControlThrowable

class ScalatraRequest(private[scalatra] val underlying: Request[IO],
                      private[scalatra] val pathParams: Map[String, Seq[String]]){

  private var cachedBody: Array[Byte] = null

  private def createBodyCache(): Unit = {
    if(cachedBody == null) {
      val bytes = underlying.body.compile.fold(List.empty[Byte]) { case (acc, byte) => acc :+ byte }
      cachedBody = bytes.unsafeRunSync().toArray
    }
  }

  lazy val body: String = {
    createBodyCache()
    val charset = underlying.contentType.flatMap(_.charset).getOrElse(Charset.`UTF-8`)
    new String(cachedBody, charset.nioCharset)
  }

  lazy val queryString: String = underlying.queryString
  lazy val contentType: Option[String] = underlying.contentType.map(_.value)
  lazy val contentLength: Option[Long] = underlying.contentLength
  lazy val headers: Map[String, String] = underlying.headers.map { x => x.name.toString() -> x.value }.toMap

  def inputStream: InputStream = {
    if(cachedBody != null) {
      new ByteArrayInputStream(cachedBody)
    } else {
      underlying.contentLength match {
        case Some(length) if length < 1024 * 1024 * 1 =>
          createBodyCache()
          new ByteArrayInputStream(cachedBody)
        case _ =>
          val tempFile = Files.createTempFile("scalatra-", ".req")
          underlying.body.through(fs2.io.file.writeAll(tempFile)).compile.drain.unsafeRunSync()
          Files.newInputStream(tempFile) // TODO Delete temp file?
      }
    }
  }
}

sealed trait ActionControlException
class HaltException(val response: Response[IO]) extends ControlThrowable with ActionControlException
class PassException extends ControlThrowable with ActionControlException

trait ScalatraBase extends ResultConverters {

  private[scalatra] val beforeActions = new ListBuffer[Action]()
  private[scalatra] val actions       = new ListBuffer[Action]()
  private[scalatra] val afterActions  = new ListBuffer[Action]()
  private[scalatra] val requestHolder = new DynamicVariable[ScalatraRequest](null)

  protected def request: ScalatraRequest = requestHolder.value

  protected def params: Map[String, String] = {
    requestHolder.value.underlying.params ++ requestHolder.value.pathParams.map { case (name, values) => name -> values.head }
  }

  protected def multiParams: Map[String, Seq[String]] = {
    requestHolder.value.underlying.multiParams ++ requestHolder.value.pathParams
  }

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

  protected def before(f: => Unit): Unit = {
    val action = new PathAction(this, None, None, unitResultConverter.convert(f))
    registerBeforeAction(action)
  }

  protected def before(path: String)(f: => Unit): Unit = {
    val action = new PathAction(this, Some(path), None, unitResultConverter.convert(f))
    registerBeforeAction(action)
  }

  protected def after(f: => Unit): Unit = {
    val action = new PathAction(this, None, None, unitResultConverter.convert(f))
    registerAfterAction(action)
  }

  protected def after(path: String)(f: => Unit): Unit = {
    val action = new PathAction(this, Some(path), None, unitResultConverter.convert(f))
    registerAfterAction(action)
  }

  protected def get[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new PathAction(this, Some(path), Some(Method.GET), converter.convert(f))
    registerAction(action)
  }

  protected def post[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new PathAction(this, Some(path), Some(Method.POST), converter.convert(f))
    registerAction(action)
  }

  protected def put[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new PathAction(this, Some(path), Some(Method.PUT), converter.convert(f))
    registerAction(action)
  }

  protected def delete[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new PathAction(this, Some(path), Some(Method.DELETE), converter.convert(f))
    registerAction(action)
  }

  protected def head[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new PathAction(this, Some(path), Some(Method.HEAD), converter.convert(f))
    registerAction(action)
  }

  protected def registerAction(action: Action): Unit = {
    actions += action
  }

  protected def registerBeforeAction(action: Action): Unit = {
    beforeActions += action
  }

  protected def registerAfterAction(action: Action): Unit = {
    afterActions += action
  }
}
