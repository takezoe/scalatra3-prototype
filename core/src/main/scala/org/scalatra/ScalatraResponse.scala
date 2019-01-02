package org.scalatra

import javax.servlet.http.HttpServletResponse

class ScalatraResponse(private[scalatra] val underlying: HttpServletResponse) {

  lazy val headers: ResponseHeaders = new ResponseHeaders(underlying)

}

class ResponseHeaders(response: HttpServletResponse){
  def set(name: String, value: String): Unit = response.setHeader(name, value)
}