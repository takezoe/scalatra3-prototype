package org.scalatra.session

import java.util.UUID

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex
import org.scalatra.ScalatraBase

import scala.collection.mutable.{Map => MutableMap}

object CookieSessionSupport {

  val CookieName = "sess"
  val SecretKey = UUID.randomUUID().toString
  val Expire = 60 // 60 minutes
  val CookieSessionKey = "org.scalatra.session.CookieSessions"

  def crypt(content: String): String = {
    val cipher = Cipher.getInstance("AES")
    val keySpec = new SecretKeySpec(SecretKey.substring(0, 16).getBytes("UTF-8"), "AES")
    cipher.init(Cipher.ENCRYPT_MODE, keySpec)
    val bytes = cipher.doFinal(content.getBytes("UTF-8"))

    Hex.encodeHexString(bytes)
  }

  def decrypt(content: String): String = {
    try {
      val decoded = Hex.decodeHex(content)

      val cipher = Cipher.getInstance("AES")
      val keySpec = new SecretKeySpec(SecretKey.substring(0, 16).getBytes("UTF-8"), "AES")
      cipher.init(Cipher.DECRYPT_MODE, keySpec)
      val bytes = cipher.doFinal(decoded)

      new String(bytes, "UTF-8")
    } catch {
      case e: Exception =>
        e.printStackTrace()
        ""
    }
  }

  def splitFirst(content: String, delimiter: String): Option[(String, String)] = {
    val i = content.indexOf(delimiter)
    if(i > 0){
      Some((content.substring(0, i), content.substring(i + delimiter.length, content.length)))
    } else {
      None
    }
  }

  private val mapper = new ObjectMapper
  mapper.registerModule(DefaultScalaModule)

  def toJson(map: Map[String, String]): String = {
    mapper.writeValueAsString(map)
  }

  def fromJson(json: String): Map[String, String] = {
    mapper.readValue(json, classOf[Map[String, String]])
  }
}

trait CookieSessionSupport extends SessionSupport {
  self: ScalatraBase =>
  import CookieSessionSupport._

  override protected def session: ScalatraSession = {
    request.get(CookieSessionKey).getOrElse {
      val params = MutableMap[String, String]()

      request.cookies.get(CookieName).foreach { cookie =>
        splitFirst(decrypt(cookie), ";") foreach {
          case (timestamp, content) =>
            if(timestamp.toLong * 1000 > System.currentTimeMillis() - (Expire * 60 * 1000)) {
              fromJson(content).foreach { case (name, value) => params.put(name, value) }
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
        val content = currentTime + ";" + toJson(session.params.toMap)
        cookies.set(CookieName, crypt(content))
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
