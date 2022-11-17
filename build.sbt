ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "smithy4s-cats",
    libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % "2.9.0",
      "com.disneystreaming.smithy4s" %% "smithy4s-core" % "0.16.8"
    )
  )
  .enablePlugins(Smithy4sCodegenPlugin)
