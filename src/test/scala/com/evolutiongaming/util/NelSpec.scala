package com.evolutiongaming.util

import org.scalatest.{FunSuite, Matchers}

class NelSpec extends FunSuite with Matchers {

  test("map") {
    val actual = for {x <- Nel(1, 2, 3)} yield x + 1
    actual shouldEqual Nel(2, 3, 4)
  }

  test("flatMap") {
    val actual = for {
      x <- Nel(1, 2, 3)
      y <- Nel(1, 2, 3)
    } yield x + y

    actual shouldEqual Nel(2, 3, 4, 3, 4, 5, 4, 5, 6)
  }

  test("size") {
    Nel(1).size shouldEqual 1
  }

  test("++") {
    Nel(1) ++ Nil shouldEqual Nel(1)
    Nel(1) ++ List(2) shouldEqual Nel(1, 2)
    Nel(1) ++ Nel(2) shouldEqual Nel(1, 2)
  }

  test("to") {
    Nel(1, 2).to[Set] shouldEqual Set(1, 2)
  }

  test("toMap") {
    Nel(1 -> "1", 2 -> "2").toMap shouldEqual Map(1 -> "1", 2 -> "2")
  }

  test(":::") {
    List(1) ::: Nel(2) shouldEqual Nel(1, 2)
  }

  test("distinct") {
    Nel(1, 2, 1, 2).distinct shouldEqual Nel(1, 2)
  }

  test("reverse") {
    Nel(1, 2).reverse shouldEqual Nel(2, 1)
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
