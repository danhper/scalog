organization := "com.tuvistavie"

name := "scalog"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.specs2" %% "specs2" % "2.3.12" % "test"
)

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:postfixOps"
)
