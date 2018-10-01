val Version = "0.0.1-SNAPSHOT"

val ScalaVersion = "2.12.6"
val Http4sVersion = "0.18.18"
val Specs2Version = "4.2.0"
val LogbackVersion = "1.2.3"

val noPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {}
)

val buildSettings = Seq[Setting[_]](
  version := Version,
  scalaVersion := ScalaVersion
)

lazy val root = project.in(file("."))
  .settings(name := "scalatra3-root")
  .settings(buildSettings)
  .settings(noPublish)
  .aggregate(core, forms, examples)

lazy val examples = project.in(file("examples"))
  .settings(name := "scalatra3-examples")
  .settings(buildSettings)
  .settings(noPublish)
  .dependsOn(core, forms, twirl)
  .enablePlugins(SbtTwirl)

lazy val json = project.in(file("json"))
  .settings(name := "scalatra3-json")
  .settings(buildSettings)
  .dependsOn(core)

lazy val forms = project.in(file("forms"))
  .settings(name := "scalatra3-forms")
  .settings(buildSettings)
  .dependsOn(core)

lazy val twirl = project.in(file("twirl"))
  .settings(
    name := "scalatra3-twirl",
    libraryDependencies ++= Seq(
      "com.typesafe.play" % "twirl-api_2.12" % "1.3.15"
    )
  )
  .settings(buildSettings)
  .dependsOn(core)

lazy val core = project.in(file("core"))
  .settings(buildSettings)
  .settings(
    name := "scalatra3-core",
    libraryDependencies ++= Seq(
      "org.http4s"             %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"             %% "http4s-circe"        % Http4sVersion,
      "org.http4s"             %% "http4s-dsl"          % Http4sVersion,
      "org.scala-lang.modules" %% "scala-xml"           % "1.1.1",
      "org.specs2"             %% "specs2-core"         % Specs2Version % "test",
      "ch.qos.logback"          % "logback-classic"     % LogbackVersion
    )
  )