organization := "com.tuvistavie"

name := "scalog"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation"
)
