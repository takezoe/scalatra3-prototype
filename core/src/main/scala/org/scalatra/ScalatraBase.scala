package org.scalatra

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.Http4sDsl

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable

class ScalatraRequest(private[scalatra] val underlying: Request[IO],
                      private[scalatra] val pathParams: Map[String, Seq[String]]){

  lazy val body = {
    val charset = underlying.contentType.flatMap(_.charset).getOrElse(Charset.`UTF-8`)
    new String(cachedBody, charset.nioCharset)
  }

  private lazy val cachedBody = {
    val bytes = underlying.body.compile.fold(List.empty[Byte]) { case (acc, byte) => acc :+ byte }
    bytes.unsafeRunSync().toArray
  }
}

trait ScalatraBase extends ActionResultTypes {

  private[scalatra] val actions       = new ListBuffer[Action]()
  private[scalatra] val requestHolder = new DynamicVariable[ScalatraRequest](null)

  protected def params: Map[String, String] = {
    requestHolder.value.underlying.params ++ requestHolder.value.pathParams.map { case (name, values) => name -> values.head }
  }

  protected def multiParams: Map[String, Seq[String]] = {
    requestHolder.value.underlying.multiParams ++ requestHolder.value.pathParams
  }

  protected def request: ScalatraRequest = requestHolder.value

  protected def get[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(this, path, Method.GET, resultType.toActionResult(f))
    registerAction(action)
  }

  protected def post[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(this, path, Method.POST, resultType.toActionResult(f))
    registerAction(action)
  }

  protected def put[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(this, path, Method.PUT, resultType.toActionResult(f))
    registerAction(action)
  }

  protected def delete[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(this, path, Method.DELETE, resultType.toActionResult(f))
    registerAction(action)
  }

  protected def head[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(this, path, Method.HEAD, resultType.toActionResult(f))
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
    val service = HttpService[IO]{ case request if app.actions.exists(_.matches(request)) =>
      val action     = app.actions.find(_.matches(request)).get
      val pathParams = action.pathParam(request)
      val result     = action.run(request, pathParams)
      IO.pure(result.toResponse())
    }
    service
  }

}
