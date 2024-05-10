name := "scala-tools"

organization := "com.evolutiongaming"

homepage := Some(new URL("http://github.com/evolution-gaming/scala-tools"))

startYear := Some(2016)

organizationName := "Evolution Gaming"

organizationHomepage := Some(url("http://evolutiongaming.com"))

bintrayOrganization := Some("evolutiongaming")

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.0", "2.12.10")

scalacOptions in (Compile,doc) ++= Seq("-groups", "-implicits", "-no-link-warnings")

resolvers += Resolver.bintrayRepo("evolutiongaming", "maven")

libraryDependencies ++= Seq(
  "com.github.t3hnar"          %% "scalax"         % "3.8.1",
  "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2",
  "com.evolutiongaming"        %% "executor-tools" % "1.0.2",
  "org.scalatest"              %% "scalatest"      % "3.0.8" % Test
)

licenses := Seq(("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")))

releaseCrossBuild := true

scalacOptsFailOnWarn := Some(false)