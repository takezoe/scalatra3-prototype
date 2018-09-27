package org.scalatra

import java.io.{ByteArrayInputStream, InputStream}

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.Http4sDsl

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable
import scala.util.control.{ControlThrowable, NonFatal}
import scala.util.control.Breaks

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

sealed trait ActionControlException
class HaltException extends ControlThrowable with ActionControlException
class RedirectException(path: String) extends ControlThrowable with ActionControlException
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

  protected def halt(): Unit = {
    throw new HaltException()
  }

  protected def redirect(path: String): Unit = {
    throw new RedirectException(path)
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

object Http4s extends Http4sDsl[IO] {

  /**
   * Builds a http4s service from a Scalatra application.
   *
   * @param app the Scalatra application
   * @return the http4s service
   */
  def buildService(app: ScalatraBase): HttpService[IO] = {
    val service = HttpService[IO]{ case request if app.actions.exists(_.matches(request)) =>

      // before actions
      val beforeActions = app.beforeActions.filter(_.matches(request))
      runActions(beforeActions, request)

      // body action
      try {
        val actions = app.actions.filter(_.matches(request))
        IO.pure(runActions(actions, request))

      } catch {
        case NonFatal(e) => throw e
      } finally {
        // after actions
        val afterActions = app.afterActions.filter(_.matches(request))
        runActions(afterActions, request)
      }
    }
    service
  }

  private def runActions(actions: Seq[Action], request: Request[IO]): Response[IO] = {
    val result = actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request)
        val result     = action.run(request, pathParams)
        Some(result.toResponse())
      } catch {
        case _: HaltException     => Some(UnitResultConverter.convert(()).toResponse())
        case _: PassException     => None
        case e: RedirectException => ???
      }
    }.find(_.isDefined).flatten

    result match {
      case Some(x) => x
      case None    => org.scalatra.NotFound()(UnitResultConverter).toResponse()
    }
  }

}
