val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name                                                   := "aoc",
    version                                                := "0.1.0-SNAPSHOT",
    scalaVersion                                           := scala3Version,
    libraryDependencies += "org.scalactic"                 %% "scalactic" % "3.2.14",
    libraryDependencies += "org.scalatest"                 %% "scalatest" % "3.2.14" % "test",
    libraryDependencies += "org.typelevel"                 %% "cats-core" % "2.9.0",
    libraryDependencies += "com.softwaremill.sttp.client3" %% "core"      % "3.8.5"
  )
