package org.scalatra.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object JsonUtil {

  private val mapper = new ObjectMapper
  mapper.registerModule(DefaultScalaModule)

  def serializeMap(map: Map[String, String]): String = {
    mapper.writeValueAsString(map)
  }

  def deserializeMap(json: String): Map[String, String] = {
    mapper.readValue(json, classOf[Map[String, String]])
  }

}
