name := "play-doc"

version := "0.1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "1.4.0"
)

lazy val root = (project in file("."))
