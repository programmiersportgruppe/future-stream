scalaVersion := "2.11.1"

libraryDependencies ++=
  "org.scala-lang" % "scala-compiler" % scalaVersion.value ::
  Nil

libraryDependencies ++= {
  "org.scalatest" %% "scalatest" % "2.2.0-M1" ::
  Nil
}.map(_ % "test")
