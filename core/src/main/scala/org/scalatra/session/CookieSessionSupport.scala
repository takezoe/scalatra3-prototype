package org.scalatra.session

import org.scalatra.ScalatraBase
import org.scalatra.util.{CryptUtil, JsonUtil, StringUtil}

import scala.collection.mutable.{Map => MutableMap}

object CookieSessionSupport {

  val CookieName = "sess"
  val Expire = 60 // 60 minutes
  val CookieSessionKey = "org.scalatra.session.CookieSessions"

}

trait CookieSessionSupport extends SessionSupport {
  self: ScalatraBase =>
  import CookieSessionSupport._

  override protected def session: ScalatraSession = {
    request.get(CookieSessionKey).getOrElse {
      val params = MutableMap[String, String]()

      request.cookies.get(CookieName).foreach { cookie =>
        StringUtil.splitFirst(CryptUtil.decrypt(cookie), ";") foreach {
          case (timestamp, content) =>
            if(timestamp.toLong * 1000 > System.currentTimeMillis() - (Expire * 60 * 1000)) {
              JsonUtil.deserializeMap(content).foreach { case (name, value) => params.put(name, value) }
            }
        }
      }

      val sessions = new CookieSessions(params)
      request.set(CookieSessionKey, sessions)
      sessions

    }.asInstanceOf[CookieSessions]
  }

  after {
    request.get(CookieSessionKey).foreach {
      case session: CookieSessions =>
        val currentTime = System.currentTimeMillis() / 1000
        val content = currentTime + ";" + JsonUtil.serializeMap(session.params.toMap)
        cookies.set(CookieName, CryptUtil.crypt(content))
    }
  }
}

class CookieSessions(val params: MutableMap[String, String]) extends ScalatraSession {

  override def get(name: String): Option[String] = {
    params.get(name)
  }

  override def set(name: String, value: String): Unit = {
    params.put(name, value)
  }

  override def remove(name: String): Unit = {
    params.remove(name)
  }

  override def invalidate(): Unit = {
    params.clear()
  }
}
