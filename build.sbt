addCommandAlias("fmt", "; scalafmtAll ; scalafmtSbt")
addCommandAlias("fmtCheck", "; scalafmtCheckAll ; scalafmtSbtCheck")

name := "B4SMT-project"
scalaVersion := "2.13.14"

val chiselVersion = "6.4.0"
val chiselTestVersion = "6.0.0"

val commonSettings = Seq(
  scalaVersion := "2.13.13",
  scalacOptions := Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-language:reflectiveCalls",
    "-Ymacro-annotations",
    "-JXss512m",
    "-JXmx2G",
  ),
//  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  libraryDependencies := Seq("org.chipsalliance" %% "chisel" % chiselVersion),
  addCompilerPlugin(
    "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full,
  ),
  Test / logBuffered := false,
  Test / parallelExecution := false,
)

lazy val pextExecutor = (project in file("pext"))
  .settings(commonSettings, name := "B4SMT-PExtentiosnExecutor")
  .dependsOn(chiselFormal)

lazy val chiselFormal = (project in file("chisel-formal"))
  .settings(
    commonSettings,
    name := "B4SMT-ChiselFormal",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18",
  )

lazy val b4smt = (project in file("b4smt"))
  .settings(
    commonSettings,
    name := "B4SMT",
    libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % chiselTestVersion % "test",
  )
  .dependsOn(pextExecutor, chiselFormal)
