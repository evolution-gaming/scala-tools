package com.evolutiongaming.util

import com.evolutiongaming.util.Validation._
import org.scalactic.Equality
import org.scalatest.{Assertions, FunSuite, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NoStackTrace

class FutureOptionSpec extends FunSuite with Matchers {

  implicit def ec: ExecutionContext = CurrentThreadExecutionContext

  val so: FutureOption[String] = FutureOption("")
  val no: FutureOption[String] = FutureOption(None)
  val sfo: FutureOption[String] = FutureOption(Future.successful(Some("")))
  val nfo: FutureOption[String] = FutureOption(Future.successful(None))
  val failed: FutureOption[String] = FutureOption(Future.failed(new TestException))

  test("map") {
    so.map { _ => 1 }.block shouldEqual Some(1)
    no.map { _ => 1 }.block shouldEqual None
    sfo.map { _ => 1 }.block shouldEqual Some(1)
    nfo.map { _ => 1 }.block shouldEqual None

    interceptFo[TestException](so.map(_ => exception))
    interceptFo[TestException](sfo.map(_ => exception))
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
    so.filter(_ == "").block shouldEqual Some("")
    so.filter(_ == " ").block shouldEqual None
    no.filter(_ == "").block shouldEqual None
    sfo.filter(_ == "").block shouldEqual Some("")
    sfo.filter(_ == " ").block shouldEqual None
    nfo.filter(_ == "").block shouldEqual None
  }

  test("filterNot") {
    so.filterNot(_ == "").block shouldEqual None
    so.filterNot(_ == " ").block shouldEqual Some("")
    no.filterNot(_ == "").block shouldEqual None
    sfo.filterNot(_ == "").block shouldEqual None
    sfo.filterNot(_ == " ").block shouldEqual Some("")
    nfo.filterNot(_ == "").block shouldEqual None
  }

  test("withFilter") {
    (for {x <- so if x == ""; if x == ""} yield x).block shouldEqual Some("")
    (for {x <- no if x == ""; if x == ""} yield x).block shouldEqual None
    (for {x <- sfo if x == ""; if x == ""} yield x).block shouldEqual Some("")
    (for {x <- nfo if x == ""; if x == ""} yield x).block shouldEqual None

    (for {
      x <- so if x == ""
      x <- sfo if x == ""
      x <- no if x == ""
      x <- nfo if x == ""
    } yield x).block shouldEqual None
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
    Future.successful("").fo.block shouldEqual Some("")
    Future.successful(null: String).fo.block shouldEqual None
    Future.successful(Some("")).fo.block shouldEqual Some("")
    Future.successful(None).fo.block shouldEqual None
  }

  test("collect") {
    so.collect { case "" => 1 }.block shouldEqual Some(1)
    so.collect { case " " => 1 }.block shouldEqual None
    sfo.collect { case "" => 1 }.block shouldEqual Some(1)
    sfo.collect { case " " => 1 }.block shouldEqual None
    no.collect { case "" => 1 }.block shouldEqual None
    nfo.collect { case "" => 1 }.block shouldEqual None
  }

  test("collectWith") {
    so.collectWith { case "" => so }.block shouldEqual Some("")
    so.collectWith { case "" => nfo }.block shouldEqual None
    so.collectWith { case " " => no }.block shouldEqual None
    so.collectWith { case " " => sfo }.block shouldEqual None
    so.collectWith { case "" => sfo }.block shouldEqual Some("")

    sfo.collectWith { case "" => so }.block shouldEqual Some("")
    sfo.collectWith { case "" => nfo }.block shouldEqual None
    sfo.collectWith { case " " => no }.block shouldEqual None
    sfo.collectWith { case " " => sfo }.block shouldEqual None
    sfo.collectWith { case "" => sfo }.block shouldEqual Some("")

    no.collect { case "" => 1 }.block shouldEqual None
    no.collect { case " " => 1 }.block shouldEqual None
    nfo.collect { case "" => 1 }.block shouldEqual None
    nfo.collect { case "" => 1 }.block shouldEqual None
  }

  test("orError") {
    so.orError().block shouldEqual ""
    sfo.orError().block shouldEqual ""
    interceptF(no.orError(new TestException))
    interceptF(nfo.orError(new TestException))
  }

  test("empty") {
    FutureOption.empty.block shouldEqual None
  }

  test("recover") {
    (so recover { case _: TestException => " " }).block shouldEqual Some("")
    (sfo recover { case _: TestException => " " }).block shouldEqual Some("")
    (no recover { case _: TestException => " " }).block shouldEqual None
    (nfo recover { case _: TestException => " " }).block shouldEqual None
    (failed recover { case _: TestException => " " }).block shouldEqual Some(" ")
  }

  test("recoverWith") {
    (so recoverWith { case _: TestException => " ".fo }).block shouldEqual Some("")
    (sfo recoverWith { case _: TestException => " ".fo }).block shouldEqual Some("")
    (no recoverWith { case _: TestException => " ".fo }).block shouldEqual None
    (nfo recoverWith { case _: TestException => " ".fo }).block shouldEqual None
    (failed recoverWith { case _: TestException => " ".fo }).block shouldEqual Some(" ")

    (so recoverWith { case _: TestException => FutureOption.empty }).block shouldEqual Some("")
    (sfo recoverWith { case _: TestException => FutureOption.empty }).block shouldEqual Some("")
    (no recoverWith { case _: TestException => FutureOption.empty }).block shouldEqual None
    (nfo recoverWith { case _: TestException => FutureOption.empty }).block shouldEqual None
    (failed recoverWith { case _: TestException => FutureOption.empty }).block shouldEqual None
  }

  test("toString") {
    so.toString shouldEqual "FutureOption(Some())"
    sfo.toString shouldEqual "FutureOption(Some())"
    no.toString shouldEqual "FutureOption(None)"
    nfo.toString shouldEqual "FutureOption(None)"
    failed.toString shouldEqual "FutureOption(com.evolutiongaming.util.FutureOptionSpec$TestException: test)"
    val promise = Promise[Option[String]]
    FutureOption(promise.future).toString shouldEqual "FutureOption(<not completed>)"
  }

  test("foreach") {
    var x = ""

    so foreach { _ => x = "so" }
    x shouldEqual "so"

    sfo foreach { _ => x = "sfo" }
    x shouldEqual "sfo"

    no foreach { _ => x = "no" }
    x shouldEqual "sfo"

    nfo foreach { _ => x = "nfo" }
    x shouldEqual "sfo"
  }

  test("sequence") {
    val list = List("".fo, no, sfo, nfo)
    FutureOption.sequence(list).block shouldEqual List("", "")
  }

  test("traverse") {
    val list = List("".fo, no, sfo, nfo)
    FutureOption.traverse(list)(identity).block shouldEqual List("", "")
  }

  private val timeout = 10.seconds

  def exception = throw new TestException

  def interceptFo[T](f: => FutureOption[T]): Unit = {
    val x = f
    Assertions.intercept[TestException] { x.block }
  }

  def interceptF[T](f: => Future[T]): Unit = {
    val x = f
    Assertions.intercept[TestException] { x.block }
  }

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
