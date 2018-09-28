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
    val service = HttpService[IO]{ case request if app.actions.exists(_.matches(request)) =>

      // before actions
      val beforeActions = app.beforeActions.filter(_.matches(request))
      runActions(beforeActions, request)

      // body action
      try {
        val actions = app.actions.filter(_.matches(request))
        IO.pure(runActions(actions, request))

      } catch {
        case NonFatal(e) => throw e
      } finally {
        // after actions
        val afterActions = app.afterActions.filter(_.matches(request))
        runActions(afterActions, request)
      }
    }
    service
  }

  private def runActions(actions: Seq[Action], request: Request[IO]): Response[IO] = {
    val result = actions.view.map { action =>
      try {
        val pathParams = action.pathParam(request)
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

}
