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

  get("/test"){
    ActionResult(201, "testtest", Map.empty)
  }
}

