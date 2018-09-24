package org.scalatra

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.Http4sDsl

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable

trait ScalatraBase {
  private[scalatra] val actions = new ListBuffer[Action]()
  private[scalatra] val requestHolder = new DynamicVariable[Request[IO]](null)
  private[scalatra] val pathParamsHolder = new DynamicVariable[Map[String, String]](null)

  protected def params: Map[String, String] = requestHolder.value.params ++ pathParamsHolder.value

  implicit protected val stringResultType = StringActionResultType

  protected def get[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(this, path, Method.GET, resultType.toActionResult(f))
    actions += action
  }
}

object Http4s extends Http4sDsl[IO] {

  def buildService(app: ScalatraBase): HttpService[IO] = {
    val service = HttpService[IO]{ case request if app.actions.exists(_.matches(request)) =>
      val action = app.actions.find(_.matches(request)).get
      val pathParams = action.pathParam(request)
      println("pathParams: " + pathParams)

      val result = action.run(request, pathParams)
      IO.pure(result.toResponse())
    }
    service
  }

}


trait Action {
  def matches(request: Request[IO]): Boolean
  def pathParam(request: Request[IO]): Map[String, String]
  def run(request: Request[IO], pathParams: Map[String, String]): ActionResult
}

class PathAction(instance: ScalatraBase, path: String, method: Method, f: => ActionResult) extends Action {

  private val pathFragments = path.split("/")

  override def matches(request: Request[IO]): Boolean = {
    println("path: " + path)
    println("pathInfo: " + request.pathInfo)

    if(method == request.method){
      val requestPathFragments = request.pathInfo.split("/")
      checkPath(pathFragments, requestPathFragments, Map.empty, false)._1
    } else false
  }

  override def pathParam(request: Request[IO]): Map[String, String] = {
    val requestPathFragments = request.pathInfo.split("/")
    checkPath(pathFragments, requestPathFragments, Map.empty, true)._2
  }

  private def checkPath(pathFragments: Seq[String], requestPathFragments: Seq[String],
                        pathParams: Map[String, String], collectPathParams: Boolean): (Boolean, Map[String, String]) = {
    (pathFragments.headOption, requestPathFragments.headOption) match {
      case (Some(a), Some(b)) if a.startsWith(":") =>
        if (collectPathParams){
          checkPath(pathFragments.tail, requestPathFragments.tail, pathParams ++ Map(a.substring(1) -> b), collectPathParams)
        } else {
          checkPath(pathFragments.tail, requestPathFragments.tail, pathParams, collectPathParams)
        }
      case (Some(a), Some(b)) if a == b =>
        checkPath(pathFragments.tail, requestPathFragments.tail, pathParams, collectPathParams)

      case (None, None) =>
        (true, pathParams)

      case _ =>
        (false, pathParams)
    }

  }

  override def run(request: Request[IO], pathParams: Map[String, String]): ActionResult = {
    instance.requestHolder.withValue(request){
      instance.pathParamsHolder.withValue(pathParams){
        f
      }
    }
  }
}
