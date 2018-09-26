package org.scalatra.examples

import org.scalatra.{ActionResult, Ok, ScalatraBase}

class HelloController extends ScalatraBase {
  before {
    println("** before **")
  }

  after {
    println("** after **")
  }

  get("/hello/:name"){
    s"Hello ${params("name")}!"
  }

  get("/test/*"){
    val paths: Seq[String] = multiParams("splat")
    Ok(paths.mkString(", "))
  }

  post("/test"){
    println("** body **")
    ActionResult(200, request.body, Map.empty)
  }
}

