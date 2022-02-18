name := "chisel-verification-examples"
version := "0.1"
scalaVersion := "2.13.5"

scalacOptions ++= Seq(
  "-language:reflectiveCalls",
  "-deprecation",
  "-feature",
  "-Xcheckinit",
)

// SNAPSHOT repositories
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.5.1"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.1" % Test
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.1" cross CrossVersion.full)

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
