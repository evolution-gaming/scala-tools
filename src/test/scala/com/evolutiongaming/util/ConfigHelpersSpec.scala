package com.evolutiongaming.util

import com.evolutiongaming.util.ConfigHelpers._
import com.typesafe.config.ConfigFactory
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class ConfigHelpersSpec extends WordSpec with Matchers {
  "ConfigHelpers" should {
    "duration" in {
      config duration "duration" shouldEqual 1.second
    }

    "durationOpt" in {
      config durationOpt "duration" shouldEqual Some(1.second)
      config durationOpt "unknown" shouldEqual None
    }

    "intOpt" in {
      config intOpt "int" shouldEqual Some(1)
      config intOpt "unknown" shouldEqual None
    }

    "booleanOpt" in {
      config booleanOpt "boolean" shouldEqual Some(true)
      config booleanOpt "unknown" shouldEqual None
    }

    "stringOpt" in {
      config stringOpt "str" shouldEqual Some("str")
      config stringOpt "unknown" shouldEqual None
    }

    "intPrefixed" in {
      config.intPrefixed("int", "prefix") shouldEqual 2
      config.intPrefixed("int", "unknown") shouldEqual 1
    }

    "stringPrefixed" in {
      config.stringPrefixed("str", "prefix") shouldEqual "strstr"
      config.stringPrefixed("str", "unknown") shouldEqual "str"
    }

    "durationPrefixed" in {
      config.durationPrefixed("duration", "prefix") shouldEqual 2.seconds
      config.durationPrefixed("duration", "unknown") shouldEqual 1.second
    }
  }

  val config = ConfigFactory.parseString(
    """
      |duration = 1s
      |int = 1
      |boolean = true
      |str = "str"
      |prefix.int = 2
      |prefix.duration = 2s
      |prefix.str = strstr
    """.stripMargin)
}
