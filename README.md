# scala-tools [![Build Status](https://travis-ci.org/evolution-gaming/scala-tools.svg)](https://travis-ci.org/evolution-gaming/scala-tools) [![Coverage Status](https://coveralls.io/repos/evolution-gaming/scala-tools/badge.svg)](https://coveralls.io/r/evolution-gaming/scala-tools) [ ![version](https://api.bintray.com/packages/evolutiongaming/maven/scala-tools/images/download.svg) ](https://bintray.com/evolutiongaming/maven/scala-tools/_latestVersion)

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
resolvers += Resolver.bintrayRepo("evolutiongaming", "maven")

libraryDependencies += "com.evolutiongaming" %% "scala-tools" % "1.6"
```
