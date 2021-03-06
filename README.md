# scalatra3-prototype

Prototype of Scalatra3 which will be fully restructured on clean code base.

Basically Scalatra3 respects existing Scalatra API, but it will elliminate ambiguous API and servlet dependent API. Also it will elliminate excessive flexibility and abstraction to keep framework simple and lightweight.

Sample application is [here](https://github.com/takezoe/scalatra3-prototype/blob/master/examples/src/main/scala/org/scalatra/examples/ScalatraSampleApp.scala). You can run this application by `sbt examples/run`.

```scala
import org.scalatra.launcher.ScalatraApp

object HelloApp extends ScalatraApp {
  get("hello"){
    Ok("Hello World!")
  }
}
```