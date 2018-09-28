package org.scalatra.examples

import org.scalatra.{ActionResult, Ok, ScalatraBase}

class HelloController extends ScalatraBase {
  before {
    println("** before **")
  }

  after {
    println("** after **")
  }

  get("/"){
    <html>
      <head>
        <title>Scalatra3 Example</title>
      </head>
      <body>
        <h1>Scalatra3 Example</h1>
      </body>
    </html>
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

