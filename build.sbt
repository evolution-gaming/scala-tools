name := "scala-tools"

organization := "com.evolutiongaming"

homepage := Some(new URL("http://github.com/evolution-gaming/scala-tools"))

startYear := Some(2016)

organizationName := "Evolution Gaming"

organizationHomepage := Some(url("http://evolutiongaming.com"))

bintrayOrganization := Some("evolutiongaming")

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.12.6", "2.11.12")

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-deprecation",
//  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Xfuture"
)

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits", "-no-link-warnings")

resolvers += Resolver.bintrayRepo("evolutiongaming", "maven")

libraryDependencies ++= Seq(
  "com.github.t3hnar" %% "scalax" % "3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0",
  "com.evolutiongaming" %% "executor-tools" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

licenses := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))

releaseCrossBuild := true