package org.scalatra

class HelloController extends ScalatraBase {
  get("/hello/:name"){
    println("Hello World!")
    s"Hello ${params("name")}!"
  }
}

