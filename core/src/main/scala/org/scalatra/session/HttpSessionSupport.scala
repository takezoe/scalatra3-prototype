package org.scalatra.session

import javax.servlet.http.{HttpServletRequest, HttpSession}
import org.scalatra.{ScalatraBase, ScalatraException}

trait HttpSessionSupport extends SessionSupport {
  self: ScalatraBase =>

  override protected def session: ScalatraSession = {
    new HttpSessions(request.underlying)
  }
}

class HttpSessions(request: HttpServletRequest) extends ScalatraSession {

  private def getHttpSession(): Option[HttpSession] = {
    Option(request.getSession)
  }

  override def get(name: String): Option[String] = {
    getHttpSession().flatMap { session =>
      Option(session.getAttribute(name)).map(_.asInstanceOf[String])
    }
  }

  override def set(name: String, value: String): Unit = {
    getHttpSession() match {
      case Some(session) => session.setAttribute(name, value)
      case None => throw new ScalatraException("session is not available.")
    }
  }

  override def remove(name: String): Unit = {
    getHttpSession().foreach { session =>
      session.removeAttribute(name)
    }
  }

  override def invalidate(): Unit = {
    getHttpSession().foreach { session =>
      session.invalidate()
    }
  }
}

