package org.scalatra

import javax.servlet.ServletConfig
import javax.servlet.http.{Cookie, HttpServlet, HttpServletRequest, HttpServletResponse}

import scala.util.control.NonFatal
import scala.collection.JavaConverters._

class ScalatraServlet(app: ScalatraBase) extends HttpServlet with ResultConverters {

  override def init(config: ServletConfig): Unit = {
    val map = config.getInitParameterNames.asScala.map { name =>
      name -> config.getInitParameter(name)
    }.toMap

    app.init(map)
  }

  override def service(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    val beforeActions = app.beforeActions.reverse.filter(_.matches(req))
    val request  = new ScalatraRequest(req)
    val response = new ScalatraResponse(resp)
    runAllActions(beforeActions, request, response) match {
      // when before actions return response then respond it
      case Some(result) => {
        renderResponse(request, response, result)
      }
      // when before actions return nothing then run body action
      case None =>
        val result = try {
          val actions = app.actions.reverse.filter(_.matches(req))
          runActions(actions, request, response)
        } catch {
          case NonFatal(e) => throw e
        } finally {
          // run after actions
          val afterActions = app.afterActions.reverse.filter(_.matches(req))
          runAllActions(afterActions, request, response)
        }
        renderResponse(request, response, result)
    }
  }


  private def runActions(actions: Seq[Action[_]], request: ScalatraRequest, response: ScalatraResponse): ActionResult = {
    val result = actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request.underlying)
        val result = action.run(request, response, pathParams)
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

  private def runAllActions(actions: Seq[Action[_]], request: ScalatraRequest, response: ScalatraResponse): Option[ActionResult] = {
    actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request.underlying)
        action.run(request, response, pathParams)
        None
      } catch {
        case e: HaltException => Some(e.result)
        case _: PassException => None
      }
    }.find(_.isDefined).flatten
  }

  private def renderResponse(request: ScalatraRequest, response: ScalatraResponse, result: ActionResult): Unit = {
    response.underlying.setStatus(result.status)
    if(result.contentType != null){
      response.underlying.setContentType(result.contentType)
    }
    result.headers.foreach { case (name, value) =>
      response.underlying.setHeader(name, value)
    }
    request.cookies.sweetCookies.foreach { case (name, content) =>
      response.underlying.addCookie(new Cookie(name, content))
    }

    val out = response.underlying.getOutputStream
    result.body.writeTo(out)
    out.flush()
  }

}
