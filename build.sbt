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
  .aggregate(core)

lazy val core = project.in(file("core"))
  .settings(buildSettings)
  .settings(
    name := "scalatra3-core",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "org.specs2"      %% "specs2-core"         % Specs2Version % "test",
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion
    )
  )