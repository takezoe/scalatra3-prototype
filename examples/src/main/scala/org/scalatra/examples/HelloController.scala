package org.scalatra.examples

import org.scalatra._
import org.scalatra.forms._
import org.scalatra.i18n.I18nSupport
import org.scalatra.twirl.TwirlSupport

class HelloController extends ScalatraBase with FormSupport with I18nSupport with TwirlSupport with FileUploadSupport {

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

  get("/twirl"){
    html.hello(new java.util.Date)
  }

  get("/hello/:name"){
    s"Hello ${params("name")}!"
  }

  get("/test/*"){
    val paths: Seq[String] = multiParams("splat")
    Ok(
      paths.map(x => s"<li>${x}</li>").mkString("<ul>", "\n", "</ul>"),
      contentType = "text/html"
    )
  }

  post("/test"){
    println("** body **")
    ActionResult(200, request.body, Map.empty)
  }

  get("/cookie") {
    val previous = cookies.get("counter") match {
      case Some(v) =>  v.toInt
      case None    => 0
    }
    cookies.update("counter", (previous + 1).toString)
    <p>
      Hi, you have been on this page {previous} times already
    </p>
  }

  get("/upload"){
    <html>
      <head>
        <title>File upload</title>
      </head>
      <body>
        <h1>File upload</h1>
        <form method="POST" action="/upload" enctype="multipart/form-data">
          <input type="text" name="fileName"></input>
          <input type="file" name="file"></input>
          <input type="submit" value="Login"></input>
        </form>
      </body>
    </html>
  }

  post("/upload"){
    println(fileParams("fileName").value)
    fileParams("file").file.map { file =>
      Ok(file, "image/png")
    }.getOrElse {
      BadRequest()
    }
  }

}

