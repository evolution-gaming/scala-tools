package com.evolutiongaming.util

import com.evolutiongaming.util.CollectionUtils._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.{Duration => _, _}

class CollectionUtilsSpec extends AnyWordSpec with Matchers {

  "CollectionUtils.MapOpsStrict" should {

    val map =
      Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")

    "provide strict version of filterKeys" in {
      map.filterKeysStrict(_ % 2 == 0) shouldBe Map(2 -> "two", 4 -> "four")
    }
    "provide strict version of mapValues" in {
      map.mapValuesStrict(_.head) shouldBe Map(1 -> 'o', 2 -> 't', 3 -> 't', 4 -> 'f', 5 -> 'f')
    }
  }
}