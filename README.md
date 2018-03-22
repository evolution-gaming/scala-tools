# scala-tools [![Build Status](https://travis-ci.org/evolution-gaming/scala-tools.svg)](https://travis-ci.org/evolution-gaming/scala-tools) [![Coverage Status](https://coveralls.io/repos/evolution-gaming/scala-tools/badge.svg)](https://coveralls.io/r/evolution-gaming/scala-tools) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/40726a1197d14196b34560902a8aea3e)](https://www.codacy.com/app/evolution-gaming/scala-tools?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=evolution-gaming/scala-tools&amp;utm_campaign=Badge_Grade) [ ![version](https://api.bintray.com/packages/evolutiongaming/maven/scala-tools/images/download.svg) ](https://bintray.com/evolutiongaming/maven/scala-tools/_latestVersion) [[License](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

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

libraryDependencies += "com.evolutiongaming" %% "scala-tools" % "1.15"
```
