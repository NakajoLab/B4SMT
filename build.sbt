ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(name := "B4-processor")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls",
  "-Ymacro-annotations",
  "-JXss512m",
  "-JXmx2G",
//  "-quickfix:any"
)

Test / logBuffered := false
Test / parallelExecution := false

scalaVersion := "2.13.14"

addCompilerPlugin(
  "org.chipsalliance" % "chisel-plugin" % "6.2.0" cross CrossVersion.full,
)
libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % "6.2.0",
  "edu.berkeley.cs" %% "chiseltest" % "6.0.0" % "test",
  "org.scalatest" %% "scalatest" % "3.2.18",
)
