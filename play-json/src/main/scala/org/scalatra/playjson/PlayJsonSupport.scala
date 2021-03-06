package org.scalatra.playjson

import org.scalatra.{ActionResult, ByteArrayBody, ResultConverter, ScalatraBase}
import play.api.libs.json._
import play.api.libs.json.{Json => PlayJson}


object PlayJsonSupport {
  private val RequestAttributeParsedBodyKey = "org.scalatra.playjson.parsedBody"
}

trait PlayJsonSupport { self: ScalatraBase =>
  import PlayJsonSupport._

  def validateJson[T] = new {
    def apply[R1, R2](hasErrors: JsError => R1, success: T => R2)
      (implicit errorsConverter: ResultConverter[R1], successConverter: ResultConverter[R2], reads: Reads[T]): ActionResult = {
      try {
        parsedBody.validate[T] match {
          case e: JsError      => errorsConverter.convert(hasErrors(e))
          case s: JsSuccess[T] => successConverter.convert(success(s.value))
        }
      } catch {
        case e: Exception => errorsConverter.convert(hasErrors(JsError(e.toString)))
      }
    }
  }

  def parsedBody: JsValue = {
    request.get(RequestAttributeParsedBodyKey).getOrElse {
      val json = PlayJson.parse(request.body)
      request.set(RequestAttributeParsedBodyKey, json)
      json
    }.asInstanceOf[JsValue]
  }

  implicit object JsValueResultConverter extends ResultConverter[JsValue] {
    def convert(result: JsValue): ActionResult = {
      ActionResult(
        status = 200,
        body = ByteArrayBody(PlayJson.stringify(result).getBytes("UTF-8")),
        contentType = "application/json",
        headers = Map.empty
      )
    }
  }

  implicit object JsonResultConverter extends ResultConverter[Json] {
    def convert(result: Json): ActionResult = {
      ActionResult(
        status = 200,
        body = ByteArrayBody(result.value.getBytes("UTF-8")),
        contentType = "application/json",
        headers = Map.empty
      )
    }
  }
}
