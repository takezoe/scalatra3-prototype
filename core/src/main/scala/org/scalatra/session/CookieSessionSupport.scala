package org.scalatra.session

import org.scalatra.ScalatraBase
import org.scalatra.util.{CryptUtils, JsonUtils, StringUtils}

import scala.collection.mutable.{Map => MutableMap}

object CookieSessionSupport {
  private val CookieName = Option(System.getProperty("org.scalatra.session.cookie.name")).getOrElse("sess")
  private val Expire = Option(System.getProperty("org.scalatra.session.cookie.expire")).getOrElse("60").toLong // 60min
  private val RequestAttributeSessionKey = "org.scalatra.session.cookie.session"
}

trait CookieSessionSupport extends SessionSupport {
  self: ScalatraBase =>
  import CookieSessionSupport._

  override protected def session: ScalatraSession = {
    request.get(RequestAttributeSessionKey).getOrElse {
      val params = MutableMap[String, String]()

      request.cookies.get(CookieName).foreach { cookie =>
        StringUtils.splitFirst(CryptUtils.decrypt(cookie), ";") foreach {
          case (timestamp, content) =>
            if(timestamp.toLong * 1000 > System.currentTimeMillis() - (Expire * 60 * 1000)) {
              JsonUtils.deserializeMap(content).foreach { case (name, value) => params.put(name, value) }
            }
        }
      }

      val sessions = new CookieSessions(params)
      request.set(RequestAttributeSessionKey, sessions)
      sessions

    }.asInstanceOf[CookieSessions]
  }

  after {
    request.get(RequestAttributeSessionKey).foreach {
      case session: CookieSessions =>
        val currentTime = System.currentTimeMillis() / 1000
        val content = currentTime + ";" + JsonUtils.serializeMap(session.params.toMap)
        cookies.set(CookieName, CryptUtils.crypt(content))
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
