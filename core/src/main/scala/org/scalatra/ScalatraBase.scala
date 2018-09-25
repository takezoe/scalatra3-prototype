package org.scalatra

import java.io.{ByteArrayInputStream, InputStream}

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.Http4sDsl

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable
import scala.util.control.NonFatal

class ScalatraRequest(private[scalatra] val underlying: Request[IO],
                      private[scalatra] val pathParams: Map[String, Seq[String]]){

  private var cachedBody: Array[Byte] = null

  lazy val body: String = {
    if(cachedBody == null) {
      val bytes = underlying.body.compile.fold(List.empty[Byte]) { case (acc, byte) => acc :+ byte }
      cachedBody = bytes.unsafeRunSync().toArray
    }
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
      ???
    }
  }
}

trait ScalatraBase extends ResultConverters {

  private[scalatra] val actions       = new ListBuffer[Action]()
  private[scalatra] val requestHolder = new DynamicVariable[ScalatraRequest](null)

  protected def request: ScalatraRequest = requestHolder.value

  protected def params: Map[String, String] = {
    requestHolder.value.underlying.params ++ requestHolder.value.pathParams.map { case (name, values) => name -> values.head }
  }

  protected def multiParams: Map[String, Seq[String]] = {
    requestHolder.value.underlying.multiParams ++ requestHolder.value.pathParams
  }

  protected def before(f: => Unit): Unit = {
    val action = new BeforeAction(this, None, None, unitResultConverter.convert(f))
    registerAction(action)
  }

  protected def before(path: String)(f: => Unit): Unit = {
    val action = new BeforeAction(this, Some(path), None, unitResultConverter.convert(f))
    registerAction(action)
  }

  protected def after(f: => Unit): Unit = {
    val action = new AfterAction(this, None, None, unitResultConverter.convert(f))
    registerAction(action)
  }

  protected def after(path: String)(f: => Unit): Unit = {
    val action = new AfterAction(this, Some(path), None, unitResultConverter.convert(f))
    registerAction(action)
  }

  protected def get[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new BodyAction(this, path, Method.GET, converter.convert(f))
    registerAction(action)
  }

  protected def post[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new BodyAction(this, path, Method.POST, converter.convert(f))
    registerAction(action)
  }

  protected def put[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new BodyAction(this, path, Method.PUT, converter.convert(f))
    registerAction(action)
  }

  protected def delete[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new BodyAction(this, path, Method.DELETE, converter.convert(f))
    registerAction(action)
  }

  protected def head[T](path: String)(f: => T)(implicit converter: ResultConverter[T]) = {
    val action = new BodyAction(this, path, Method.HEAD, converter.convert(f))
    registerAction(action)
  }

  protected def registerAction(action: Action): Unit = {
    actions += action
  }
}

object Http4s extends Http4sDsl[IO] {

  /**
   * Builds a http4s service from a Scalatra application.
   *
   * @param app the Scalatra application
   * @return the http4s service
   */
  def buildService(app: ScalatraBase): HttpService[IO] = {
    val service = HttpService[IO]{ case request if app.actions.exists(x => x.isInstanceOf[BodyAction] && x.matches(request)) =>

      // before actions
      val beforeActions = app.actions.filter(x => x.isInstanceOf[BeforeAction] && x.matches(request))
      beforeActions.foreach { action =>
        val pathParams = action.pathParam(request)
        println("find before: " + action)
        action.run(request, pathParams)
      }

      // body action
      try {
        val action     = app.actions.find(x => x.isInstanceOf[BodyAction] && x.matches(request)).get
        val pathParams = action.pathParam(request)
        val result     = action.run(request, pathParams)
        IO.pure(result.toResponse())

      } catch {
        case NonFatal(e) => throw e

      } finally {
        // after actions
        val afterActions = app.actions.filter(x => x.isInstanceOf[AfterAction] && x.matches(request))
        afterActions.foreach { action =>
          val pathParams = action.pathParam(request)
          action.run(request, pathParams)
        }
      }
    }
    service
  }

}
