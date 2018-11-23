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
  .aggregate(core, forms, launcher, examples)

lazy val examples = project.in(file("examples"))
  .settings(name := "scalatra3-examples")
  .settings(buildSettings)
  .settings(noPublish)
  .dependsOn(core, forms, twirl, launcher)
  .enablePlugins(SbtTwirl)

lazy val launcher = project.in(file("launcher"))
  .settings(name := "scalatra3-launcher")
  .settings(buildSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.eclipse.jetty"      %  "jetty-server"       % jettyVersion,
      "org.eclipse.jetty"      %  "jetty-servlet"      % jettyVersion,
      "org.eclipse.jetty"      %  "jetty-webapp"       % jettyVersion
    )
  )
  .dependsOn(core)

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

val jettyVersion = "9.4.6.v20170531"

lazy val core = project.in(file("core"))
  .settings(buildSettings)
  .settings(
    name := "scalatra3-core",
    libraryDependencies ++= Seq(
//      "org.eclipse.jetty"      %  "jetty-server"       % jettyVersion,
//      "org.eclipse.jetty"      %  "jetty-servlet"      % jettyVersion,
//      "org.eclipse.jetty"      %  "jetty-webapp"       % jettyVersion,
      "javax.servlet"          %  "javax.servlet-api"  % "3.1.0" % "provided",
      "org.scala-lang.modules" %% "scala-xml"          % "1.1.1",
      "org.specs2"             %% "specs2-core"        % Specs2Version % "test",
      "ch.qos.logback"         %  "logback-classic"    % LogbackVersion,
      "commons-fileupload"     %  "commons-fileupload" % "1.3.3"
    )
  )