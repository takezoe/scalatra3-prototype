package org.scalatra

import cats.effect.IO
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

import scala.collection.mutable.ListBuffer

trait ScalatraBase {
  private[scalatra] val actions = new ListBuffer[Action]()

  implicit protected val stringResultType = StringActionResultType

  protected def get[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(path, Method.GET, resultType.toActionResult(f))
    actions += action
  }
}

object Http4s extends Http4sDsl[IO] {

  def buildService(actions: Seq[Action]): HttpService[IO] = {
    val service = HttpService[IO]{ case request if actions.exists(_.matches(request)) =>
      val action = actions.find(_.matches(request)).get
      val result = action.run()

      println("params: " + request.params)

      IO.pure(result.toResponse())
    }
    service
  }

}


trait Action {
  def matches(request: Request[IO]): Boolean
  def run(): ActionResult
}

class PathAction(path: String, method: Method, f: => ActionResult) extends Action {
  override def matches(request: Request[IO]): Boolean = {
    println("path: " + path)
    println("pathInfo: " + request.pathInfo)
    method == request.method && path == request.pathInfo
  }
  override def run(): ActionResult = f
}
