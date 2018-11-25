package org.scalatra.playjson

import play.api.libs.json._
import play.api.libs.json.{Json => PlayJson}

case class Json(value: String)

object Json {
  def apply[T](value: T)(implicit writes: Writes[T]): Json = {
    Json(PlayJson.stringify(PlayJson.toJson(value)))
  }
}