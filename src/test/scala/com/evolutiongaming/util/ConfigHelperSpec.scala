package com.evolutiongaming.util

import com.evolutiongaming.util.ConfigHelper._
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._

class ConfigHelperSpec extends FunSuite with Matchers {

  test("get") {
    val config = ConfigFactory.parseString("key:value")
    config.get[String]("key") shouldEqual "value"
    intercept[Exception] {
      config.get[Int]("key")
    }
  }

  test("get boolean") {
    val config = ConfigFactory.parseString("key:true")
    config.get[Boolean]("key") shouldEqual true
  }

  test("get config") {
    val config = ConfigFactory.parseString("key:{key: value}")
    config.get[Config]("key").getString("key") shouldEqual "value"
  }

  test("get duration") {
    val config = ConfigFactory.parseString("key:1m")
    config.get[FiniteDuration]("key") shouldEqual 1.minute
  }

  test("get int list") {
    val config = ConfigFactory.parseString("key:[1,2]")
    config.get[List[Int]]("key") shouldEqual List(1, 2)
  }

  test("get duration list") {
    val config = ConfigFactory.parseString("key:[1s,2s]")
    config.get[List[FiniteDuration]]("key") shouldEqual List(1.second, 2.second)
  }

  test("get double list") {
    val config = ConfigFactory.parseString("key:[1.1,2.2]")
    config.get[List[Double]]("key") shouldEqual List(1.1, 2.2)
  }

  test("get string list") {
    val config = ConfigFactory.parseString("key:[a,b]")
    config.get[List[String]]("key") shouldEqual List("a", "b")
  }

  test("get boolean list") {
    val config = ConfigFactory.parseString("key:[true,false]")
    config.get[List[Boolean]]("key") shouldEqual List(true, false)
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
