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
resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.6-SNAPSHOT"
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.6-SNAPSHOT" % Test
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.6-SNAPSHOT" cross CrossVersion.full)

Compile / scalaSource := baseDirectory.value / "src"
Test    / scalaSource := baseDirectory.value / "test"
