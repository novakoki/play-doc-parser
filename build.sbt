name := "play-doc"

version := "0.1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "1.4.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val root = (project in file("."))
