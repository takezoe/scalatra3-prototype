package org.scalatra.session

trait SessionSupport {
  protected def session: ScalatraSession
}

trait ScalatraSession {
  def get(name: String): Option[String]
  def set(name: String, value: String): Unit
  def remove(name: String): Unit
  def invalidate(): Unit
}
