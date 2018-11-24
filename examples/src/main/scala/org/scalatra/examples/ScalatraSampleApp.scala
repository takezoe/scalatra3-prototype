package org.scalatra.examples

import org.scalatra._
import org.scalatra.forms._
import org.scalatra.i18n.I18nSupport
import org.scalatra.launcher.ScalatraApp
import org.scalatra.session.CookieSessionSupport
import org.scalatra.twirl.TwirlSupport
import org.scalatra.util.JsonUtil

object ScalatraSampleApp extends ScalatraApp with FormSupport with I18nSupport with FileUploadSupport with TwirlSupport with CookieSessionSupport {

  case class JsonForm(user: User)
  case class User(name: String, mail: Seq[String])

  case class LoginForm(
    id: String,
    pass: String
  )

  val form = mapping(
    "id"   -> label("Id", text(required, maxlength(10))),
    "pass" -> label("Password", text(required, maxlength(10)))
  )(LoginForm.apply)

  val jsonForm = mapping(
    "user" -> mapping(
      "name" -> text(required),
      "mail" -> list(text())
    )(User.apply)
  )(JsonForm.apply)

  before {
    println("** before **")
  }

  after {
    println("** after **")
  }


  get("/"){
    html.hello(new java.util.Date)
  }

  get("/form"){
    <html>
      <body>
        <h1>Form</h1>
        <form method="POST" action="/form">
          <input type="text" name="id"></input>
          <input type="password" name="pass"></input>
          <input type="submit" value="Login"></input>
        </form>
      </body>
    </html>
  }

  post("/form"){
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

  get("/upload"){
    <html>
      <body>
        <h1>File upload</h1>
        <form method="POST" action="/upload" enctype="multipart/form-data">
          <input type="text" name="fileName"></input>
          <input type="file" name="file"></input>
          <input type="submit" value="Upload"></input>
        </form>
      </body>
    </html>
  }

  post("/upload"){
    fileParams.get("file").map { file =>
      Ok(file.getInputStream, "image/png")
    }.getOrElse {
      BadRequest()
    }
  }

  get("/cookie") {
    val previous = cookies.get("counter") match {
      case Some(v) => v.toInt
      case None    => 0
    }
    cookies.set("counter", (previous + 1).toString)
    <html>
      <body>
        <h1>Cookie</h1>
        <p>Hi, you have been on this page {previous} times already</p>
      </body>
    </html>
  }

  get("/session") {
    val previous = session.get("counter") match {
      case Some(v) => v.toInt
      case None    => 0
    }
    session.set("counter", (previous + 1).toString)
    <html>
      <body>
        <h1>Session</h1>
        <p>Hi, you have been on this page {previous} times already</p>
      </body>
    </html>
  }

  post("/json"){
    validateJson(jsonForm)(
      (errors: Seq[(String, String)]) => {
        BadRequest(
          body = JsonUtil.serialize(errors),
          contentType = "application/json"
        )
      },
      (form: JsonForm) => {
        BadRequest(
          body = JsonUtil.serialize(form),
          contentType = "application/json"
        )
      }
    )
  }

//  get("/hello/:name"){
//    s"Hello ${params("name")}!"
//  }
//
//  get("/test/*"){
//    val paths: Seq[String] = multiParams("splat")
//    Ok(
//      paths.map(x => s"<li>${x}</li>").mkString("<ul>", "\n", "</ul>"),
//      contentType = "text/html"
//    )
//  }
//
  post("/test"){
    println("** body **")
    request.body
  }

}

