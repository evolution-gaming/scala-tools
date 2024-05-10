package com.evolutiongaming.concurrent

import org.scalatest.{FunSuite, Matchers}

class AtomicRefSpec extends FunSuite with Matchers {

  test("updateAndGet") {
    val ref = AtomicRef(0)
    ref.updateAndGet(_ + 1) shouldEqual 1
    ref() shouldEqual 1
  }

  test("getAndUpdate") {
    val ref = AtomicRef(0)
    ref.getAndUpdate(_ + 1) shouldEqual 0
    ref() shouldEqual 1
  }

  test("apply") {
    val ref = AtomicRef(0)
    ref() shouldEqual 0
  }

  test("set") {
    val ref = AtomicRef(0)
    ref.set(1)
    ref() shouldEqual 1
  }

  test("getAndSet") {
    val ref = AtomicRef(0)
    ref.getAndSet(1) shouldEqual 0
    ref() shouldEqual 1
  }

  test("compareAndSet") {
    val ref = AtomicRef(0)
    ref.compareAndSet(0, 1) shouldEqual true
    ref() shouldEqual 1

    ref.compareAndSet(0, 1) shouldEqual false
    ref() shouldEqual 1
  }

  test("toString") {
    val ref = AtomicRef(0)
    ref.toString shouldEqual "AtomicRef(0)"
  }
}
