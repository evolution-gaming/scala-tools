package com.evolutiongaming.util

import org.scalatest.{FunSuite, Matchers}

class NelSpec extends FunSuite with Matchers {
  test("map") {
    val actual = for {x <- Nel(1, 2, 3)} yield x + 1
    actual shouldEqual Nel(2, 3, 4)
  }

  test("flatMap")  {
    val actual = for {
      x <- Nel(1, 2, 3)
      y <- Nel(1, 2, 3)
    } yield x + y

    actual shouldEqual Nel(2, 3, 4, 3, 4, 5, 4, 5, 6)
  }

  test("toString") {
    Nel(1, 2, 3).toString shouldEqual "Nel(1, 2, 3)"

    1 :: Nel

    1 :: 1 :: Nel
  }

  test("1 :: Nel") {
    1 :: Nel shouldEqual Nel(1)
  }

  test("1 :: 2 :: Nel") {
    1 :: 2 :: Nel shouldEqual Nel(1, 2)
  }
}
