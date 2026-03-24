val scala3Version = "3.8.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "fp-strategies",
    version := "0.1",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.2.4" % Test
  )
