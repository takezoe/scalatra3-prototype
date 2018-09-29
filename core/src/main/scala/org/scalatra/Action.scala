package org.scalatra

import cats.effect.IO
import org.http4s.{Method, Request}

class Action(instance: ScalatraBase, path: Option[String], method: Option[Method], f: => StreamActionResult) {

  protected val pathFragments = path.map(_.split("/")).getOrElse(Array.empty)

  /**
   * Tests a requested path matches this action.
   *
   * @param request the request object of http4s
   * @return true if matches, false otherwise
   */
  def matches(request: Request[IO]): Boolean = {
    method match {
      case Some(x) if x == request.method =>
        if(pathFragments.nonEmpty){
          val requestPathFragments = request.pathInfo.split("/")
          checkPath(pathFragments, requestPathFragments, Map.empty, false)._1
        } else true
      case None =>
        if(pathFragments.nonEmpty){
          val requestPathFragments = request.pathInfo.split("/")
          checkPath(pathFragments, requestPathFragments, Map.empty, false)._1
        } else true
      case _ => false
    }
  }

  /**
   * Runs this action with a given request and path parameters.
   *
   * @param request the request
   * @param pathParams the path parameters
   * @return the result of this action
   * @throws HaltException when halt() is called in the action
   * @throws PassException when pass() is called in the action
   */
  def run(request: ScalatraRequest, pathParams: Map[String, Seq[String]]): StreamActionResult = {
    instance.requestHolder.withValue(request){
      instance.pathParamHolder.withValue(pathParams){
        f
      }
    }
  }

  /**
   * Extracts parameters from a given requested path.
   *
   * @param request the request object of http4s
   * @return the path parameters
   */
  def pathParam(request: Request[IO]): Map[String, Seq[String]] = {
    val requestPathFragments = request.pathInfo.split("/")
    checkPath(pathFragments, requestPathFragments, Map.empty, true)._2
  }

  protected def checkPath(pathFragments: Seq[String], requestPathFragments: Seq[String],
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

  protected def addMultiParams(name: String, value: String, params: Map[String, Seq[String]]): Map[String, Seq[String]] = {
    params.get(name) match {
      case Some(x) => params ++ Map(name -> (x :+ value))
      case None    => params ++ Map(name -> Seq(value))
    }
  }

}
