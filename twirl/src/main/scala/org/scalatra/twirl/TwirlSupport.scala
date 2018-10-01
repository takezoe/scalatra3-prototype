package org.scalatra.twirl

import org.scalatra.{ResultConverter, StreamActionResult}
import play.twirl.api.Html

trait TwirlSupport {

  implicit object HtmlResultConverter extends ResultConverter[Html] {
    def convert(result: Html): StreamActionResult = {
      StreamActionResult(
        status = 200,
        body = fs2.Stream(result.body.getBytes("UTF-8"): _*),
        contentType = "text/html; charset=UTF-8",
        headers = Map.empty
      )
    }
  }


}
