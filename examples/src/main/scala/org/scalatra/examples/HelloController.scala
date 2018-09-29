package org.scalatra.examples

import org.scalatra._
import org.scalatra.forms._
import org.scalatra.i18n.I18nSupport

class HelloController extends ScalatraBase with FormSupport with I18nSupport {

  case class LoginForm(
    id: String,
    pass: String
  )

  val form = mapping(
    "id"   -> label("Id", text(required, maxlength(10))),
    "pass" -> label("Password", text(required, maxlength(10)))
  )(LoginForm.apply)

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
        <form method="POST" action="/login">
          <input type="text" name="id"></input>
          <input type="password" name="pass"></input>
          <input type="submit" value="Login"></input>
        </form>
      </body>
    </html>
  }

  post("/login"){
    validate(form)(
      (errors: Seq[(String, String)]) => {
        println(errors)
        redirect("/")
      },
      (form: LoginForm) => {
        Ok(s"Hello ${form.id}!")
      }
    )
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

