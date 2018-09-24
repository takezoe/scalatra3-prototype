package org.scalatra

class HelloController extends ScalatraBase {
  get("/hello/:name"){
    s"Hello ${params("name")}!"
  }

  get("/test/*"){
    val paths: Seq[String] = multiParams("splat")
    Ok(paths.mkString(", "))
  }

  post("/test"){
    ActionResult(200, request.body, Map.empty)
  }
}

