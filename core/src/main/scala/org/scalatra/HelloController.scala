package org.scalatra

class HelloController extends ScalatraBase {
  get("/hello/:name"){
    println("Hello World!")
    s"Hello ${params("name")}!"
  }

  get("/test/*"){
    println(multiParams("splat"))
    Ok("Hello!!")
  }

  post("/test"){
    ActionResult(200, request.body, Map.empty)
  }
}

