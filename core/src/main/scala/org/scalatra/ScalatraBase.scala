package org.scalatra

import cats.effect.IO
import io.circe._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

import scala.collection.mutable.ListBuffer
import org.http4s.server.blaze.BlazeBuilder
import fs2.StreamApp

trait ScalatraBase {
  val actions = new ListBuffer[Action]()

  def get(path: String)(f: => ActionResult) = {
    val action = new PathAction(path, HttpMethod.Get, f)
    actions += action
  }
}

object Http4s extends Http4sDsl[IO] {

  def buildService(actions: Seq[Action]): HttpService[IO] = {
    val service = HttpService[IO]{ case request if actions.exists(_.matches(request)) =>
      val action = actions.find(_.matches(request)).get
      val result = action.run()
      IO.pure(result.toResponse())
    }
    service
  }

}


trait Action {
  def matches(request: Request[IO]): Boolean
  def run(): ActionResult
}

class PathAction(path: String, method: HttpMethod, f: => ActionResult) extends Action {
  override def matches(request: Request[IO]): Boolean = {
    println("path: " + path)
    println("pathInfo: " + request.pathInfo)
    path == request.pathInfo
  }
  override def run(): ActionResult = f
}

case class ActionResult(status: Int){
  def toResponse(): Response[IO] = Response[IO](status = Status(status))
}

sealed trait HttpMethod
object HttpMethod {
  case object Get extends HttpMethod
}
