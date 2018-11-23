package org.scalatra.twirl

import org.scalatra.{ActionResult, ByteArrayBody, ResultConverter}
import play.twirl.api.Html

trait TwirlSupport {

  implicit object HtmlResultConverter extends ResultConverter[Html] {
    def convert(result: Html): ActionResult = {
      ActionResult(
        status = 200,
        body = ByteArrayBody(result.body.getBytes("UTF-8")),
        contentType = "text/html; charset=UTF-8",
        headers = Map.empty
      )
    }
  }


}
