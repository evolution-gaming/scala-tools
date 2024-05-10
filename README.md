# scala-tools
[![Build Status](https://github.com/evolution-gaming/scala-tools/workflows/CI/badge.svg)](https://github.com/evolution-gaming/scala-tools/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/evolution-gaming/scala-tools/badge.svg)](https://coveralls.io/r/evolution-gaming/scala-tools)
[![Version](https://img.shields.io/badge/version-click-blue)](https://evolution.jfrog.io/artifactory/api/search/latestVersion?g=com.evolutiongaming&a=scala-tools-actor_2.13&repos=public)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellowgreen.svg)](https://opensource.org/licenses/MIT)

### ConfigHelpers.scala

```scala
  import com.evolutiongaming.util.ConfigHelpers._

  config duration "path" // FiniteDuration

  config durationOpt "path" // Option[FiniteDuration]

  config intOpt "path" // Option[Int]

  config booleanOpt "path" // Option[Boolean]

  config stringOpt "path" // Option[String]
```

## Setup

```scala
addSbtPlugin("com.evolution" % "sbt-artifactory-plugin" % "0.0.2")

libraryDependencies += "com.evolutiongaming" %% "scala-tools" % "3.0.5"
```
