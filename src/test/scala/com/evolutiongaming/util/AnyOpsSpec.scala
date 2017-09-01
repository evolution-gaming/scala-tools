package com.evolutiongaming.util

import com.evolutiongaming.util.Validation._
import org.scalatest.{FunSuite, Matchers}

import scala.util.Success

class AnyOpsSpec extends FunSuite with Matchers {
  test("future") {
    "test".future.value shouldEqual Some(Success("test"))
  }
}
