package com.evolutiongaming.util

import com.evolutiongaming.util.Validation._
import org.scalatest.{FunSuite, Matchers}

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
    val fe = FutureEither("right") flatMap { _ => sys error "error" }
    intercept[RuntimeException] {
      await(fe.future)
    }
  }

  test("map") {
    val fe = FutureEither("right") map { _ => sys error "error" }
    intercept[RuntimeException] {
      await(fe.future)
    }
  }

  test("result") {
    FutureEither("right") result timeout shouldEqual "right".ok
    FutureEither("left".ko) result timeout shouldEqual "left".ko
    FutureEither(Future successful "right".ok) result timeout shouldEqual "right".ok
  }

  test("leftMap") {
    FutureEither[String, String]("right") leftMap { _ => 0 } result timeout shouldEqual "right".ok
    FutureEither[String, String]("left".ko) leftMap { _ => 0 } result timeout shouldEqual 0.ko
    FutureEither[String, String](Future successful "right".ok) leftMap { _ => 0 } result timeout shouldEqual "right".ok
    FutureEither[String, String](Future successful "left".ko) leftMap { _ => 0 } result timeout shouldEqual 0.ko
  }

  test("fold") {
    await(FutureEither[String, String]("right").fold(_ => 1, _ => 2)) shouldEqual 2
    await(FutureEither[String, String]("left".ko).fold(_ => 1, _ => 2)) shouldEqual 1
    await(FutureEither[String, String](Future successful "right".ok).fold(_ => 1, _ => 2)) shouldEqual 2
    await(FutureEither[String, String](Future successful "left".ko).fold(_ => 1, _ => 2)) shouldEqual 1
  }

  test("getOrElse") {
    await(FutureEither[String, String]("right") getOrElse { _ => "l" }) shouldEqual "right"
    await(FutureEither[String, String]("left".ko) getOrElse { _ => "l" }) shouldEqual "l"
    await(FutureEither[String, String](Future successful "right".ok) getOrElse { _ => "l" }) shouldEqual "right"
    await(FutureEither[String, String](Future successful "left".ko) getOrElse { _ => "l" }) shouldEqual "l"
  }

  test("either") {
    await(FutureEither[String, String]("right").either) shouldEqual "right".ok
    await(FutureEither[String, String]("left".ko).either) shouldEqual "left".ko
    await(FutureEither[String, String](Future successful "right".ok).either) shouldEqual "right".ok
    await(FutureEither[String, String](Future successful "left".ko).either) shouldEqual "left".ko
  }

  test("recover") {
    val fe = FutureEither[String, String](Future.failed(new RuntimeException)) recover { case _: RuntimeException => "left".ko }
    fe.result(timeout) shouldEqual "left".ko
  }

  val timeout = 10.seconds

  def await[T](awaitable: Awaitable[T]) = Await.result(awaitable, timeout)
}
