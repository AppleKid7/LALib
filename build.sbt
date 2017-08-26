import Dependencies._


lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "LALib",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.specs2" %% "specs2-core" % "3.9.4" % Test,
      "junit" % "junit" % "4.12" % Test,
      "org.specs2" %% "specs2-junit" % "3.9.4" % Test,
      "com.novocode" % "junit-interface" % "0.11" % "test->default",
      "org.mockito" % "mockito-core" % "2.8.47"
    )
  )