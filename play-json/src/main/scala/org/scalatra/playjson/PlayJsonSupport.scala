package org.scalatra.playjson

import org.scalatra.{ActionResult, ByteArrayBody, ResultConverter, ScalatraBase}
import play.api.libs.json._
import play.api.libs.json.{Json => PlayJson}


object PlayJsonSupport {
  private val RequestAttributeParsedBodyKey = "org.scalatra.playjson.PlayJsonSupport.parsedBody"
}

trait PlayJsonSupport { self: ScalatraBase =>
  import PlayJsonSupport._

  protected def validateJson[T, R1, R2](hasErrors: JsError => R1, success: T => R2)
    (implicit errorsConverter: ResultConverter[R1], successConverter: ResultConverter[R2], reads: Reads[T]): ActionResult = {
    parsedBody.validate[T] match {
      case e: JsError      => errorsConverter.convert(hasErrors(e))
      case s: JsSuccess[T] => successConverter.convert(success(s.value))
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
