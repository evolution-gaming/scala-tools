package com.evolutiongaming.util

import java.time.{Duration, Instant, LocalTime}

import scala.concurrent.duration.{Duration => _, _}
import TemporalUtils._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TemporalUtilsSpec extends AnyWordSpec with Matchers {

  "TemporalUtils" should {
    "add and subtract java.time.Duration" in {
      val instant = Instant.parse("2019-02-03T10:15:30.00Z")
      (instant + Duration.ofSeconds(41)) shouldBe Instant.parse("2019-02-03T10:16:11.00Z")
      (instant - Duration.ofDays(2)) shouldBe Instant.parse("2019-02-01T10:15:30.00Z")

      val localTime = LocalTime.of(10, 20, 30)
      (localTime + Duration.ofSeconds(5)) shouldBe LocalTime.of(10, 20, 35)
      (localTime - Duration.ofMinutes(41)) shouldBe LocalTime.of(9, 39, 30)
    }

    "add and subtract FiniteDuration" in {
      val instant = Instant.parse("2019-02-03T10:15:30.00Z")
      (instant + 41.second) shouldBe Instant.parse("2019-02-03T10:16:11.00Z")
      (instant - 2.days) shouldBe Instant.parse("2019-02-01T10:15:30.00Z")

      val localTime = LocalTime.of(10, 20, 30)
      (localTime + 5.seconds) shouldBe LocalTime.of(10, 20, 35)
      (localTime - 41.minutes) shouldBe LocalTime.of(9, 39, 30)
    }

    "get duration between temporals" in {
      val instant1 = Instant.parse("2019-02-03T10:15:30.00Z")
      val instant2 = Instant.parse("2019-02-13T10:15:30.00Z")
      (instant2 - instant1) shouldBe Duration.ofDays(10)

      val localTime1 = LocalTime.of(10, 20, 30)
      val localTime2 = LocalTime.of(8, 20, 30)
      (localTime2 - localTime1) shouldBe Duration.ofHours(-2)
    }

    "convert Duration to FiniteDuration" in {
      Duration.ofSeconds(17).toScala shouldBe 17.seconds
      Duration.ofMinutes(120).toScala shouldBe 2.hours
    }

    "create Instant from epoch millis" in {
      200.toInstant shouldBe Instant.EPOCH.plusMillis(200)
    }
  }
}