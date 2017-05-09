package com.evolutiongaming.util

import org.scalactic.Equality
import org.scalatest.{FunSuite, Matchers}
import com.evolutiongaming.util.Validation._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NoStackTrace

class FutureOptionSpec extends FunSuite with Matchers {

  implicit def ec: ExecutionContext = CurrentThreadExecutionContext

  val so = FutureOption("")
  val no = FutureOption(None)
  val sfo = FutureOption(Future.successful(Some("")))
  val nfo = FutureOption(Future.successful(None))

  test("map") {
    so.map { _ => 1 }.block shouldEqual Some(1)
    no.map { _ => 1 }.block shouldEqual None
    sfo.map { _ => 1 }.block shouldEqual Some(1)
    nfo.map { _ => 1 }.block shouldEqual None
  }

  test("flatMap") {
    (for {x <- so; y <- so} yield 1).block shouldEqual Some(1)

    (for {x <- so; y <- no} yield 1).block shouldEqual None
    (for {x <- no; y <- so} yield 1).block shouldEqual None
    (for {x <- no; y <- no} yield 1).block shouldEqual None
    (for {x <- so; y <- nfo} yield 1).block shouldEqual None
    (for {x <- nfo; y <- so} yield 1).block shouldEqual None

    (for {x <- sfo; y <- sfo} yield 1).block shouldEqual Some(1)

    (for {x <- sfo; y <- nfo} yield 1).block shouldEqual None
    (for {x <- nfo; y <- sfo} yield 1).block shouldEqual None
    (for {x <- nfo; y <- nfo} yield 1).block shouldEqual None
    (for {x <- sfo; y <- no} yield 1).block shouldEqual None
    (for {x <- no; y <- sfo} yield 1).block shouldEqual None

    (for {x <- so; y <- sfo} yield 1).block shouldEqual Some(1)
    (for {x <- sfo; y <- so} yield 1).block shouldEqual Some(1)
  }

  test("fold") {
    so.fold(1)(_ => 2).block shouldEqual 2
    no.fold(1)(_ => 2).block shouldEqual 1
    sfo.fold(1)(_ => 2).block shouldEqual 2
    no.fold(1)(_ => 2).block shouldEqual 1
  }

  test("future") {
    so.future.block shouldEqual Some("")
    no.future.block shouldEqual None
    sfo.future.block shouldEqual Some("")
    nfo.future.block shouldEqual None
  }

  test("orElse") {
    so.orElse(FutureOption(1)).block shouldEqual Some("")
    no.orElse(FutureOption(1)).block shouldEqual Some(1)
    sfo.orElse(FutureOption(1)).block shouldEqual Some("")
    nfo.orElse(FutureOption(1)).block shouldEqual Some(1)
  }

  test("getOrElse") {
    so.getOrElse(1).block shouldEqual ""
    no.getOrElse(1).block shouldEqual 1
    sfo.getOrElse(1).block shouldEqual ""
    nfo.getOrElse(1).block shouldEqual 1
  }

  test("|") {
    so.|(1).block shouldEqual ""
    no.|(1).block shouldEqual 1
    sfo.|(1).block shouldEqual ""
    nfo.|(1).block shouldEqual 1
  }

  test("exists") {
    so.exists(_ == "").block shouldEqual true
    no.exists(_ == "").block shouldEqual false
    sfo.exists(_ == "").block shouldEqual true
    nfo.exists(_ == "").block shouldEqual false
  }

  test("contains") {
    so.contains("").block shouldEqual true
    no.contains("").block shouldEqual false
    sfo.contains("").block shouldEqual true
    nfo.contains("").block shouldEqual false
  }

  test("forall") {
    so.forall(_ == "").block shouldEqual true
    no.forall(_ == "").block shouldEqual true
    sfo.forall(_ == "").block shouldEqual true
    nfo.forall(_ == "").block shouldEqual true
  }

  test("await") {
    so.await(timeout) shouldEqual Some("")
    no.await(timeout) shouldEqual None
    sfo.await(timeout) shouldEqual Some("")
    nfo.await(timeout) shouldEqual None
  }

  test("filter") {
    so.filter( _ == "").block shouldEqual Some("")
    so.filter( _ == " ").block shouldEqual None
    no.filter( _ == "").block shouldEqual None
    sfo.filter( _ == "").block shouldEqual Some("")
    sfo.filter( _ == " ").block shouldEqual None
    nfo.filter( _ == "").block shouldEqual None
  }

  test("filterNot") {
    so.filterNot( _ == "").block shouldEqual None
    so.filterNot( _ == " ").block shouldEqual Some("")
    no.filterNot( _ == "").block shouldEqual None
    sfo.filterNot( _ == "").block shouldEqual None
    sfo.filterNot( _ == " ").block shouldEqual Some("")
    nfo.filterNot( _ == "").block shouldEqual None
  }

  test("withFilter") {
    (for {x <- so if x == ""} yield x).block shouldEqual Some("")
    (for {x <- no if x == ""} yield x).block shouldEqual None
    (for {x <- sfo if x == ""} yield x).block shouldEqual Some("")
    (for {x <- nfo if x == ""} yield x).block shouldEqual None
  }

  test("isEmpty") {
    so.isEmpty.block shouldEqual false
    no.isEmpty.block shouldEqual true
    sfo.isEmpty.block shouldEqual false
    nfo.isEmpty.block shouldEqual true
  }

  test("nonEmpty") {
    so.nonEmpty.block shouldEqual true
    no.nonEmpty.block shouldEqual false
    sfo.nonEmpty.block shouldEqual true
    nfo.nonEmpty.block shouldEqual false
  }

  test("toRight") {
    so.toRight("l").block shouldEqual "".ok
    no.toRight("l").block shouldEqual "l".ko
    sfo.toRight("l").block shouldEqual "".ok
    nfo.toRight("l").block shouldEqual "l".ko
  }

  test("?>>") {
    so.?>>("l").block shouldEqual "".ok
    no.?>>("l").block shouldEqual "l".ko
    sfo.?>>("l").block shouldEqual "".ok
    nfo.?>>("l").block shouldEqual "l".ko
  }

  test("toLeft") {
    so.toLeft("r").block shouldEqual "".ko
    no.toLeft("r").block shouldEqual "r".ok
    sfo.toLeft("r").block shouldEqual "".ko
    nfo.toLeft("r").block shouldEqual "r".ok
  }

  test("fo") {
    (null: String).fo.block shouldEqual None
    "".fo.block shouldEqual Some("")
    Some("").fo.block shouldEqual Some("")
    None.fo.block shouldEqual None
  }

  private val timeout = 10.seconds

  class TestException extends RuntimeException("test") with NoStackTrace

  implicit class FutureBlock[T](self: Future[T]) {
    def block: T = Await.result(self, timeout)
  }

  implicit class FutureOptionTest[T](self: FutureOption[T]) {
    def block: Option[T] = self.future.block
  }

  implicit class FutureEitherTest[L, R](self: FutureEither[L, R]) {
    def block: Either[L, R] = self.future.block
  }

  implicit def optionEquality[T]: Equality[Option[T]] = new Equality[Option[T]] {
    def areEqual(a: Option[T], b: Any) = a == b
  }

  implicit def eitherEquality[L, R]: Equality[Either[L, R]] = new Equality[Either[L, R]] {
    def areEqual(a: Either[L, R], b: Any) = a == b
  }
}
