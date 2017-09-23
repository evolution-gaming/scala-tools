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

  test("filter") {
    (1 :: 2 :: Nel) filter { _ >= 1 } shouldEqual (1 :: 2 :: Nil)
    (1 :: 2 :: Nel) filter { _ > 1 } shouldEqual (2 :: Nil)
    (1 :: 2 :: Nel) filter { _ > 1 } shouldEqual (2 :: Nil)
    (1 :: 2 :: Nel) filter { _ > 2 } shouldEqual Nil
  }

  test("filterNot") {
    (1 :: 2 :: Nel) filterNot { _ == 3 } shouldEqual (1 :: 2 :: Nil)
    (1 :: 2 :: Nel) filterNot { _ <= 1 } shouldEqual (2 :: Nil)
    (1 :: 2 :: Nel) filterNot { _ <= 2 } shouldEqual Nil
  }

  test("mkString") {
    (1 :: 2 :: Nel).mkString shouldEqual "12"

    (1 :: 2 :: Nel).mkString(",") shouldEqual "1,2"

    (1 :: 2 :: Nel).mkString("[", ",", "]") shouldEqual "[1,2]"
  }

  test("exists") {
    (1 :: Nel) exists { _ == 1 } shouldEqual true
  }

  test("forall") {
    (1 :: Nel) forall { _ == 1 } shouldEqual true
    (1 :: 2 :: Nel) forall { _ == 1 } shouldEqual false
  }

  test("contains") {
    (1 :: Nel) contains 1 shouldEqual true
    (1 :: Nel) contains 2 shouldEqual false
    (1 :: 2 :: Nel) contains 1 shouldEqual true
    (1 :: 2 :: Nel) contains 2 shouldEqual true
  }

  test("find") {
    (1 :: Nel) find { _ == 1 } shouldEqual Some(1)
    (1 :: Nel) find { _ == 2 } shouldEqual None
  }

  test("count") {
    (1 :: Nel).count(_ == 1) shouldEqual 1
    (1 :: Nel).count(_ == 2) shouldEqual 0
  }

  test("unsafe") {
    Nel.unsafe(1 :: Nil) shouldEqual (1 :: Nel)

    intercept[Exception] {
      Nel.unsafe(Nil)
    }
  }

  test("opt") {
    Nel.opt(Nil) shouldEqual None
    Nel.opt(1 :: Nil) shouldEqual Some(1 :: Nel)
  }

  test("collect") {
    (1 :: 2 :: 1 :: Nel) collect { case 1 => "" } shouldEqual ("" :: "" :: Nil)
  }

  test("collectFirst") {
    (1 :: 2 :: 1 :: Nel) collectFirst { case 1 => "" } shouldEqual Some("")
    (1 :: Nel) collectFirst { case 2 => "" } shouldEqual None
  }

  test("last") {
    (1 :: Nel).last shouldEqual 1
    (1 :: 2 :: Nel).last shouldEqual 2
  }

  test("concat") {
    (1 :: Nel) concat (2 :: Nel) shouldEqual (1 :: 2 :: Nel)
  }

  test("foldLeft") {
    (1 :: 2 :: 3 :: Nel).foldLeft("") { (s, x) => s + x } shouldEqual "123"
  }

  test("foldRight") {
    (1 :: 2 :: 3 :: Nel).foldRight("") { (x, s) => s + x } shouldEqual "321"
  }

  test("reduceLeft") {
    (1 :: 2 :: 3 :: Nel).reduceLeft[Any] { (s, x) => s.toString + x.toString } shouldEqual "123"
  }

  test("reduceRight") {
    (1 :: 2 :: 3 :: Nel).reduceRight[Any] { (x, s) => s.toString + x.toString } shouldEqual "321"
  }

  test("+:") {
    1 +: Nel(2) shouldEqual Nel(1, 2)
  }

  test(":+") {
    (Nel(1) :+ 2) shouldEqual Nel(1, 2)
  }

  test("unzip") {
    Nel((1, 2)).unzip shouldEqual ((Nel(1), Nel(2)))
  }

  test("unzip3") {
    Nel((1, 2, 3)).unzip3 shouldEqual ((Nel(1), Nel(2), Nel(3)))
  }
}
