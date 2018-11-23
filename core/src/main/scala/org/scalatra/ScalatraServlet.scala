package org.scalatra

import javax.servlet.http.{Cookie, HttpServlet, HttpServletRequest, HttpServletResponse}

import scala.util.control.NonFatal

class ScalatraServlet(app: ScalatraBase) extends HttpServlet with ResultConverters {

  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    val beforeActions = app.beforeActions.reverse.filter(_.matches(req))
    val request = new ScalatraRequest(req)
    runAllActions(beforeActions, request) match {
      // when before actions return response then respond it
      case Some(result) => {
        renderResponse(request, resp, result)
      }
      // when before actions return nothing then run body action
      case None =>
        val result = try {
          val actions = app.actions.reverse.filter(_.matches(req))
          runActions(actions, request)
        } catch {
          case NonFatal(e) => throw e
        } finally {
          // run after actions
          val afterActions = app.afterActions.reverse.filter(_.matches(req))
          runAllActions(afterActions, request)
        }
        renderResponse(request, resp, result)
    }
  }


  private def runActions(actions: Seq[Action[_]], request: ScalatraRequest): ActionResult = {
    val result = actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request.underlying)
        val result     = action.run(request, pathParams)
        Some(result)
      } catch {
        case e: HaltException => Some(e.result)
        case _: PassException => None
      }
    }.find(_.isDefined).flatten

    result match {
      case Some(x) => x
      case None    => org.scalatra.NotFound()(UnitResultConverter)
    }
  }

  private def runAllActions(actions: Seq[Action[_]], request: ScalatraRequest): Option[ActionResult] = {
    actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request.underlying)
        action.run(request, pathParams)
        None
      } catch {
        case e: HaltException => Some(e.result)
        case _: PassException => None
      }
    }.find(_.isDefined).flatten
  }

  private def renderResponse(request: ScalatraRequest, response: HttpServletResponse, result: ActionResult): Unit = {
    response.setStatus(result.status)
    if(result.contentType != null){
      response.setContentType(result.contentType)
    }
    result.headers.foreach { case (name, value) =>
      response.setHeader(name, value)
    }
    request.cookies.sweetCookies.foreach { case (name, content) =>
      response.addCookie(new Cookie(name, content))
    }

    val out = response.getOutputStream
    result.body.writeTo(out)
    out.flush()
  }

}
