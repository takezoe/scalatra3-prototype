package org.scalatra

import cats.effect.IO
import org.http4s.{Method, Request}

trait Action {
  def matches(request: Request[IO]): Boolean
  def pathParam(request: Request[IO]): Map[String, Seq[String]]
  def run(request: Request[IO], pathParams: Map[String, Seq[String]]): ActionResult
}

class PathAction(instance: ScalatraBase, path: String, method: Method, f: => ActionResult) extends Action {

  private val pathFragments = path.split("/")

  /**
   * Tests the request path matches this action.
   *
   * @param request the request object of http4s
   * @return true if matches, false otherwise
   */
  override def matches(request: Request[IO]): Boolean = {
    println("path: " + path)
    println("pathInfo: " + request.pathInfo)

    if(method == request.method){
      val requestPathFragments = request.pathInfo.split("/")
      checkPath(pathFragments, requestPathFragments, Map.empty, false)._1
    } else false
  }

  /**
   * Extracts parameters from the given path.
   *
   * @param request the request object of http4s
   * @return the path parameters
   */
  override def pathParam(request: Request[IO]): Map[String, Seq[String]] = {
    val requestPathFragments = request.pathInfo.split("/")
    checkPath(pathFragments, requestPathFragments, Map.empty, true)._2
  }

  /**
   * Runs this action with the given request and the path parameters.
   *
   * @param request the request object of http4s
   * @param pathParams the path parameters
   * @return the result of this action
   */
  override def run(request: Request[IO], pathParams: Map[String, Seq[String]]): ActionResult = {
    instance.requestHolder.withValue(request){
      instance.pathParamsHolder.withValue(pathParams){
        f
      }
    }
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
}
