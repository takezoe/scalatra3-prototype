package org.scalatra

import javax.servlet.http.{HttpServlet, HttpServletRequest, HttpServletResponse}
import scala.util.control.NonFatal

class ScalatraServlet(app: ScalatraBase) extends HttpServlet with ResultConverters {

  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    val beforeActions = app.beforeActions.reverse.filter(_.matches(req))
    runAllActions(beforeActions, req) match {
      // when before actions return response then respond it
      case Some(result) => {
        renderResponse(resp, result)
      }
      // when before actions return nothing then run body action
      case None =>
        val result = try {
          val actions = app.actions.reverse.filter(_.matches(req))
          runActions(actions, req)
        } catch {
          case NonFatal(e) => throw e
        } finally {
          // run after actions
          val afterActions = app.afterActions.reverse.filter(_.matches(req))
          runAllActions(afterActions, req)
        }
        renderResponse(resp, result)

//            // Add sweet cookies
//            IO.pure(request.cookies.sweetCookies.foldLeft(response){ case (res, (name, content)) =>
//              println(s"Sweet cookie added: ${name}=${content}")
//              res.addCookie(Cookie(name, content))
//            })
    }
  }


  private def runActions(actions: Seq[Action[_]], request: HttpServletRequest): ActionResult = {
    val result = actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request)
        val result     = action.run(request, pathParams)
        Some(result)
      } catch {
        case e: HaltException => Some(e.response)
        case _: PassException => None
      }
    }.find(_.isDefined).flatten

    result match {
      case Some(x) => x
      case None    => org.scalatra.NotFound()(UnitResultConverter)
    }
  }

  private def runAllActions(actions: Seq[Action[_]], request: HttpServletRequest): Option[ActionResult] = {
    actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request)
        action.run(request, pathParams)
        None
      } catch {
        case e: HaltException => Some(e.response)
        case _: PassException => None
      }
    }.find(_.isDefined).flatten
  }

  private def renderResponse(response: HttpServletResponse, result: ActionResult): Unit = {
    response.setStatus(result.status)
    if(result.contentType != null){
      response.setContentType(result.contentType)
    }
    result.headers.foreach { case (name, value) =>
      response.setHeader(name, value)
    }
    val out = response.getOutputStream
    result.body.writeTo(out)
    out.flush()
  }

}
