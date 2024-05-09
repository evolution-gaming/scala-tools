package com.evolutiongaming.util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InstantStreamSpec extends AnyWordSpec with Matchers {

  "InstantStream" should {
    "preserve minimal interval" in {
      val dts = InstantStream().take(100).toList
      dts.reduceLeft { (prev, next) =>
        (next.toEpochMilli - prev.toEpochMilli) >= 10 shouldEqual true
        next
      }
    }
  }
}
