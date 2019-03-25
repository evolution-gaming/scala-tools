package com.evolutiongaming.util

import com.evolutiongaming.util.Tap._
import org.scalatest.{FunSuite, Matchers}

class TapSpec extends FunSuite with Matchers {
  test("tap") {
    0 tap { _ shouldEqual 0 }
    0 tap { _ => } shouldEqual 0
  }

  test("let") {
    0 let { _ shouldEqual 0 }
    0 let { _ + 1 } shouldEqual 1
  }

  test("|>") {
    0 |> { _ shouldEqual 0 }
    0 |> { _ + 1 } shouldEqual 1
  }
}
