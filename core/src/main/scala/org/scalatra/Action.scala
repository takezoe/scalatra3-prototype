package org.scalatra

import javax.servlet.http.HttpServletRequest
import scala.collection.mutable.{Map => MutableMap}

sealed trait Method {
  val name: String

  def matches(method: String): Boolean = {
    method.toLowerCase == this.name
  }
}

object Method {
  case object Get extends Method {
    val name = "get"
  }
  case object Post extends Method {
    val name = "post"
  }
  case object Put extends Method {
    val name = "put"
  }
  case object Delete extends Method {
    val name = "delete"
  }
  case object Head extends Method {
    val name = "head"
  }
  case object Patch extends Method {
    val name = "patch"
  }
}

class Action[T](instance: ScalatraBase, path: Option[String], method: Option[Method], f: => ActionResult) {

  protected val pathFragments = path.map(splitPath).getOrElse(Seq.empty)

  private def splitPath(path: String): Seq[String] = {
    if(path == "/"){
      Seq("")
    } else {
      path.split("/")
    }
  }

  /**
   * Tests a requested path matches this action.
   *
   * @param request the request object of http4s
   * @return true if matches, false otherwise
   */
  def matches(request: HttpServletRequest): Boolean = {
    method match {
      case Some(x) if x.matches(request.getMethod) =>
        if(pathFragments.nonEmpty){
          val requestPathFragments = splitPath(request.getPathInfo)
          checkPath(pathFragments, requestPathFragments, MutableMap.empty, false)._1
        } else true
      case None =>
        if(pathFragments.nonEmpty){
          val requestPathFragments = splitPath(request.getPathInfo)
          checkPath(pathFragments, requestPathFragments, MutableMap.empty, false)._1
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
  def run(request: ScalatraRequest, pathParams: Map[String, Seq[String]]): ActionResult = {
    request.underlying.removeAttribute(ScalatraBase.RequestAttributeParamsKey)
    request.underlying.removeAttribute(ScalatraBase.RequestAttributeMultiParamsKey)

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
  def pathParam(request: HttpServletRequest): Map[String, Seq[String]] = {
    val requestPathFragments = request.getPathInfo.split("/")
    checkPath(pathFragments, requestPathFragments, scala.collection.mutable.Map.empty, true)._2
  }

  protected def checkPath(pathFragments: Seq[String], requestPathFragments: Seq[String],
    pathParams: MutableMap[String, Seq[String]], collectPathParams: Boolean): (Boolean, Map[String, Seq[String]]) = {
    (pathFragments.headOption, requestPathFragments.headOption) match {
      case (Some(a), Some(b)) if a.startsWith(":") =>
        if (collectPathParams){
          checkPath(pathFragments.tail, requestPathFragments.tail, addMultiParams(a.substring(1), b, pathParams), collectPathParams)
        } else {
          checkPath(pathFragments.tail, requestPathFragments.tail, pathParams, collectPathParams)
        }
      case (Some(a), Some(b)) if a == "*" && pathFragments.size == 1 =>
        if (collectPathParams){
          (true, requestPathFragments.foldLeft(pathParams){ case (params, x) => addMultiParams("splat", x, params) }.toMap)
        } else {
          (true, pathParams.toMap)
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
        (true, pathParams.toMap)

      case _ =>
        (false, pathParams.toMap)
    }
  }

  protected def addMultiParams(name: String, value: String, params: MutableMap[String, Seq[String]]): MutableMap[String, Seq[String]] = {
    params.get(name) match {
      case Some(x) => params += (name -> (x :+ value))
      case None    => params += (name -> Seq(value))
    }
  }

}
