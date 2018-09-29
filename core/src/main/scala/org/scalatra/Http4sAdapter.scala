package org.scalatra

import cats.effect.IO
import org.http4s._
import org.http4s.dsl.Http4sDsl

import scala.util.control.NonFatal

object Http4sAdapter extends Http4sDsl[IO] with ResultConverters {

  /**
   * Builds a http4s service from a Scalatra application.
   *
   * @param app the Scalatra application
   * @return the http4s service
   */
  def buildService(app: ScalatraBase): HttpService[IO] = {
    val service = HttpService[IO]{ case req if app.actions.exists(_.matches(req)) =>
      val request = new ScalatraRequest(req)

      // before actions
      val beforeActions = app.beforeActions.reverse.filter(_.matches(req))
      runAllActions(beforeActions, request) match {
        // when before actions return response then respond it
        case Some(res) => IO.pure(res)
        // when before actions return nothing then run body action
        case None =>
          try {
            val actions = app.actions.reverse.filter(_.matches(req))
            IO.pure(runActions(actions, request))
          } catch {
            case NonFatal(e) => throw e
          } finally {
            // run after actions
            val afterActions = app.afterActions.reverse.filter(_.matches(req))
            runAllActions(afterActions, request)
          }
      }
    }
    service
  }

  private def runActions(actions: Seq[Action], request: ScalatraRequest): Response[IO] = {
    val result = actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request.underlying)
        val result     = action.run(request, pathParams)
        Some(result.toResponse())
      } catch {
        case e: HaltException => Some(e.response)
        case _: PassException => None
      }
    }.find(_.isDefined).flatten

    result match {
      case Some(x) => x
      case None    => org.scalatra.NotFound()(UnitResultConverter).toResponse()
    }
  }

  private def runAllActions(actions: Seq[Action], request: ScalatraRequest): Option[Response[IO]] = {
    actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request.underlying)
        action.run(request, pathParams)
        None
      } catch {
        case e: HaltException => Some(e.response)
        case _: PassException => None
      }
    }.find(_.isDefined).flatten
  }

}
