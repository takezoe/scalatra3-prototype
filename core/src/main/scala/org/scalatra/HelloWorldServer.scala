package org.scalatra

import cats.effect.IO
import fs2.StreamApp
import org.http4s.server.blaze.BlazeBuilder

class HelloController extends ScalatraBase {
  get("/hello"){
    println("Hello World!")
    ActionResult(200)
  }
}

object BlazeServer extends StreamApp[IO] {
  import scala.concurrent.ExecutionContext.Implicits.global

  def stream(args: List[String], requestShutdown: IO[Unit]) = {
    val controller = new HelloController()
    val service = Http4s.buildService(controller.actions)

    BlazeBuilder[IO]
      .bindHttp(8080, "0.0.0.0")
      .mountService(service, "/")
      .serve
  }
}
