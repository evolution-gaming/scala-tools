package com.evolutiongaming.util

import com.evolutiongaming.util.ConfigHelper._
import com.typesafe.config.ConfigFactory
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration.FiniteDuration

class ConfigHelperSpec extends FunSuite with Matchers {
  test("get") {
    val config = ConfigFactory.parseString("key:value")
    config.get[String]("key") shouldEqual "value"
    intercept[Exception] {
      config.get[Int]("key")
    }
  }

  test("getOpt") {
    val config = ConfigFactory.parseString("key:value")
    config.getOpt[String]("key") shouldEqual Some("value")
    config.getOpt[String]("unknown") shouldEqual None
    intercept[Exception] {
      config.getOpt[Long]("key")
    }
  }

  test("getOrElse") {
    val config = ConfigFactory.parseString("k1:s\nk2:1")
    config.getOrElse[String]("k1", "k2") shouldEqual "s"
    config.getOrElse[String]("unknown", "k2") shouldEqual "1"
    intercept[Exception] {
      config.getOrElse[FiniteDuration]("k1", "k2")
    }
  }

  test("getPrefixed") {
    val config = ConfigFactory.parseString("prefix.key:v1\nkey:v2")
    config.getPrefixed[String]("key", "prefix") shouldEqual "v1"
    config.getPrefixed[String]("key", "unknown") shouldEqual "v2"
    intercept[Exception] {
      config.getPrefixed[Double]("key", "prefix")
    }
  }
}
