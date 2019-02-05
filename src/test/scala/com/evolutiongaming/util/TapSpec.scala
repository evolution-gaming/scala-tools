package com.evolutiongaming.util

import com.evolutiongaming.util.Tap._
import org.scalatest.{FunSuite, Matchers}

class TapSpec extends FunSuite with Matchers {
  test("apply function to itself") {
    0 tap { _ shouldEqual 0 }
  }

  test("returns itself after function application") {
    0 tap { _ => } shouldEqual 0
  }
}
