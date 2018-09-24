package org.scalatra

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.Http4sDsl

import scala.collection.mutable.ListBuffer
import scala.util.DynamicVariable

trait ScalatraBase {
  private[scalatra] val actions = new ListBuffer[Action]()
  private[scalatra] val requestHolder    = new DynamicVariable[Request[IO]](null)
  private[scalatra] val pathParamsHolder = new DynamicVariable[Map[String, Seq[String]]](null)

  protected def params: Map[String, String] = requestHolder.value.params ++ pathParamsHolder.value.map { case (name, values) => name -> values.head }
  protected def multiParams: Map[String, Seq[String]] = requestHolder.value.multiParams ++ pathParamsHolder.value

  implicit protected val stringResultType = StringActionResultType

  protected def get[T](path: String)(f: => T)(implicit resultType: ActionResultType[T]) = {
    val action = new PathAction(this, path, Method.GET, resultType.toActionResult(f))
    registerAction(action)
  }

  protected def registerAction(action: Action): Unit = {
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
  def pathParam(request: Request[IO]): Map[String, Seq[String]]
  def run(request: Request[IO], pathParams: Map[String, Seq[String]]): ActionResult
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

  override def pathParam(request: Request[IO]): Map[String, Seq[String]] = {
    val requestPathFragments = request.pathInfo.split("/")
    checkPath(pathFragments, requestPathFragments, Map.empty, true)._2
  }

  private def checkPath(pathFragments: Seq[String], requestPathFragments: Seq[String],
                        pathParams: Map[String, Seq[String]], collectPathParams: Boolean): (Boolean, Map[String, Seq[String]]) = {
    (pathFragments.headOption, requestPathFragments.headOption) match {
      case (Some(a), Some(b)) if a.startsWith(":") =>
        if (collectPathParams){
          checkPath(pathFragments.tail, requestPathFragments.tail, addMultiParams(a.substring(1), b, pathParams), collectPathParams)
        } else {
          checkPath(pathFragments.tail, requestPathFragments.tail, pathParams, collectPathParams)
        }
      case (Some(a), Some(b)) if a == "*" && pathFragments.size == 1 =>
        if (collectPathParams){
          (true, requestPathFragments.foldLeft(pathParams){ case (params, x) =>  addMultiParams("splat", x, params) })
        } else {
          (true, pathParams)
        }
      case (Some(a), Some(b)) if a == "*" =>
        if (collectPathParams){
          checkPath(pathFragments.tail, requestPathFragments.tail, addMultiParams("splat", b, pathParams), collectPathParams)
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

  private def addMultiParams(name: String, value: String, params: Map[String, Seq[String]]): Map[String, Seq[String]] = {
    params.get(name) match {
      case Some(x) => params ++ Map(name -> (x :+ value))
      case None    => params ++ Map(name -> Seq(value))
    }
  }

  override def run(request: Request[IO], pathParams: Map[String, Seq[String]]): ActionResult = {
    instance.requestHolder.withValue(request){
      instance.pathParamsHolder.withValue(pathParams){
        f
      }
    }
  }
}
