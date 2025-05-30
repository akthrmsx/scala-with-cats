val scala3Version = "3.7.0"
val catsVersion = "2.9.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-with-cats",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
