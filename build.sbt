
name := "scala-tools"

organization := "com.evolutiongaming"

homepage := Some(url("http://github.com/evolution-gaming/scala-tools"))

startYear := Some(2016)

organizationName := "Evolution"

organizationHomepage := Some(url("http://evolution.com"))

publishTo := Some(Resolver.evolutionReleases)

scalaVersion := crossScalaVersions.value.last

crossScalaVersions := Seq("2.13.14", "2.12.19", "3.3.3")

Compile / doc / scalacOptions ++= Seq("-groups", "-implicits", "-no-link-warnings")

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.5",
  "com.evolutiongaming"        %% "executor-tools" % "1.0.4",
  "org.scalatest"              %% "scalatest"      % "3.2.18" % Test
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

releaseCrossBuild := true

scalacOptsFailOnWarn := Some(false)

//addCommandAlias("fmt", "scalafixEnable; scalafixAll; all scalafmtAll scalafmtSbt")
//addCommandAlias("check", "all Compile/doc versionPolicyCheck scalafmtCheckAll scalafmtSbtCheck; scalafixEnable; scalafixAll --check",)
addCommandAlias("check", "Compile/doc")
addCommandAlias("build", "all compile test")
