organization := "com.github.kmizu"

name := "skala"

version := "0.1.0-SNAPSHOT"

scalaVersion := "3.3.5"

libraryDependencies ++=  Seq(
  "org.playframework" %% "play-json" % "3.0.4",
   "org.scalameta" %% "munit" % "1.0.4" % Test
) 