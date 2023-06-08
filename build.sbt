ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(name := "B4-processor")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
//  "-Xfatal-warnings",
  "-language:reflectiveCalls",
  "-Ymacro-annotations"
)

//Test / logBuffered := false
//Test / parallelExecution := false

scalaVersion := "2.13.11"

addCompilerPlugin(
  "edu.berkeley.cs" % "chisel3-plugin" % "3.6.0-RC2" cross CrossVersion.full
)
libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel3" % "3.6.0-RC2",
  // We also recommend using chiseltest for writing unit tests
  "edu.berkeley.cs" %% "chiseltest" % "0.6.0-RC2" % "test"
)
