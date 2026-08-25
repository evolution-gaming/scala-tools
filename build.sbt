name := "scala-tools"

organization := "com.evolutiongaming"

homepage := Some(url("http://github.com/evolution-gaming/scala-tools"))

startYear := Some(2016)

organizationName := "Evolution"

organizationHomepage := Some(url("http://evolution.com"))

publishTo := Some(Resolver.evolutionReleases)

scalaVersion := crossScalaVersions.value.last

crossScalaVersions := Seq("2.13.18", "3.3.8")

Compile / doc / scalacOptions ++= Seq("-groups", "-implicits", "-no-link-warnings")

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "com.evolutiongaming" %% "executor-tools" % "1.0.4",
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

scalacOptsFailOnWarn := Some(false)

versionPolicyIntention := Compatibility.BinaryCompatible

addCommandAlias("check", "all scalafmtCheckRepo versionPolicyCheck Compile/doc")
addCommandAlias("fmt", "scalafmtRepo")
addCommandAlias("build", "all compile test")
