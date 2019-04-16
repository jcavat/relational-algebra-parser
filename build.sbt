lazy val commonSettings = Seq(
  name := "relational-algebra-parser",
  version := "0.1",
  organization := "ch.hepia",
  scalaVersion := "2.12.8",
  test in assembly := {}
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "2.1.0",
      "org.scalactic" %% "scalactic" % "3.0.5",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test"
    )
  ).
  settings(
    mainClass in assembly := Some("ch.hepia.Main")
  )

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)
