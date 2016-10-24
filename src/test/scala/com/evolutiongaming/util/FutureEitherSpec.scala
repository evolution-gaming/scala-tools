package com.evolutiongaming.util

import org.scalatest.{FunSuite, Matchers}

import com.evolutiongaming.util.Validation._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable, Future}

class FutureEitherSpec extends FunSuite with Matchers {

  test("merge") {
    await(FutureEither("left".ko).merge) shouldEqual "left"
    await(FutureEither("right".ko).merge) shouldEqual "right"
    await(FutureEither("right").merge) shouldEqual "right"
    await(FutureEither[String, String](Future successful "left".ko[String]).merge)
  }

  test("flatMap") {
    val fe = FutureEither("right") flatMap {_ => sys error "error"}
    intercept[RuntimeException] {
      await(fe.future)
    }
  }

  test("map") {
    val fe = FutureEither("right") map {_ => sys error "error"}
    intercept[RuntimeException] {
      await(fe.future)
    }
  }

  def await[T](awaitable: Awaitable[T]) = Await.result(awaitable, 10.seconds)
}
