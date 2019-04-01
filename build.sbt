lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization    := "ch.hepia",
      scalaVersion    := "2.12.8"
    )),
    name := "My Akka HTTP Project",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "2.1.0"
    )
  )
