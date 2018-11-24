package org.scalatra.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

import scala.reflect.ClassTag

object JsonUtil {

  private val mapper = new ObjectMapper
  mapper.registerModule(DefaultScalaModule)


  def serialize(value: Any): String = {
    mapper.writeValueAsString(value)
  }

  def deserialize[T](json: String)(implicit c: ClassTag[T]): T = {
    mapper.readValue(json, c.runtimeClass.asInstanceOf[Class[T]])
  }

}
