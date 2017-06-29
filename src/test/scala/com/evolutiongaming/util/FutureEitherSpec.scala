package com.evolutiongaming.util

import com.evolutiongaming.concurrent.CurrentThreadExecutionContext
import com.evolutiongaming.util.Validation._
import org.scalactic.Equality
import org.scalatest.{Assertions, FunSuite, Matchers}
import org.scalatest.EitherValues._
import org.scalatest.concurrent.ScalaFutures._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.Success
import scala.util.control.NoStackTrace

class FutureEitherSpec extends FunSuite with Matchers {

  implicit def ec: ExecutionContext = CurrentThreadExecutionContext

  private val l: Either[String, String] = "l".ko
  private val r: Either[String, String] = "r".ok
  private val lf: Future[Either[String, String]] = Future successful l
  private val rf: Future[Either[String, String]] = Future successful r

  private val le = FutureEither(l)
  private val re = FutureEither(r)
  private val lfe = FutureEither(lf)
  private val rfe = FutureEither(rf)

  private val fe: FutureEither[String, String] = FutureEither(Future failed new TestException)


  test("merge") {
    re.merge.block shouldEqual "r"
    rfe.merge.block shouldEqual "r"
    le.merge.block shouldEqual "l"
    lfe.merge.block shouldEqual "l"
  }

  test("map") {
    (re map { _ + "r" }).block shouldEqual "rr".ok
    (rfe map { _ + "r" }).block shouldEqual "rr".ok
    (le map { _ + "r" }).block shouldEqual l
    (lfe map { _ + "r" }).block shouldEqual l

    interceptFe(re.map { _ => exception })
    interceptFe(rfe.map { _ => exception })
    (le map { _ => exception }).block shouldEqual l
    (lfe map { _ => exception }).block shouldEqual l
  }

  test("flatMap") {
    (re flatMap { _ => re }).block shouldEqual r
    (re flatMap { _ => le }).block shouldEqual l
    (re flatMap { _ => rfe }).block shouldEqual r
    (re flatMap { _ => lfe }).block shouldEqual l

    (le flatMap { _ => re }).block shouldEqual l
    (le flatMap { _ => le }).block shouldEqual l
    (le flatMap { _ => rfe }).block shouldEqual l
    (le flatMap { _ => lfe }).block shouldEqual l

    (rfe flatMap { _ => re }).block shouldEqual r
    (rfe flatMap { _ => le }).block shouldEqual l
    (rfe flatMap { _ => rfe }).block shouldEqual r
    (rfe flatMap { _ => lfe }).block shouldEqual l

    (lfe flatMap { _ => re }).block shouldEqual l
    (lfe flatMap { _ => le }).block shouldEqual l
    (lfe flatMap { _ => rfe }).block shouldEqual l
    (lfe flatMap { _ => lfe }).block shouldEqual l

    interceptFe(re flatMap { _ => exception })
    interceptFe(rfe flatMap { _ => exception })
    (le flatMap { _ => exception }).block shouldEqual l
    (lfe flatMap { _ => exception }).block shouldEqual l
  }

  test("leftMap") {
    (re leftMap { _ + "l" }).block shouldEqual r
    (rfe leftMap { _ + "l" }).block shouldEqual r
    (le leftMap { _ + "l" }).block shouldEqual "ll".ko
    (lfe leftMap { _ + "l" }).block shouldEqual "ll".ko

    (re leftMap { _ => exception }).block shouldEqual r
    (rfe leftMap { _ => exception }).block shouldEqual r
    interceptFe(le leftMap { _ => exception })
    interceptFe(lfe leftMap { _ => exception })
  }

  test("leftFlatMap") {
    (re leftFlatMap { _ => re }).block shouldEqual r
    (re leftFlatMap { _ => le }).block shouldEqual r
    (re leftFlatMap { _ => rfe }).block shouldEqual r
    (re leftFlatMap { _ => lfe }).block shouldEqual r

    (le leftFlatMap { _ => re }).block shouldEqual r
    (le leftFlatMap { _ => le }).block shouldEqual l
    (le leftFlatMap { _ => rfe }).block shouldEqual r
    (le leftFlatMap { _ => lfe }).block shouldEqual l

    (rfe leftFlatMap { _ => re }).block shouldEqual r
    (rfe leftFlatMap { _ => le }).block shouldEqual r
    (rfe leftFlatMap { _ => rfe }).block shouldEqual r
    (rfe leftFlatMap { _ => lfe }).block shouldEqual r

    (lfe leftFlatMap { _ => re }).block shouldEqual r
    (lfe leftFlatMap { _ => le }).block shouldEqual l
    (lfe leftFlatMap { _ => rfe }).block shouldEqual r
    (lfe leftFlatMap { _ => lfe }).block shouldEqual l

    (re leftFlatMap { _ => exception }).block shouldEqual r
    (rfe leftFlatMap { _ => exception }).block shouldEqual r
    interceptFe(le leftFlatMap { _ => exception })
    interceptFe(lfe leftFlatMap { _ => exception })
  }

  test("await") {
    re await timeout shouldEqual r
    le await timeout shouldEqual l
    rfe await timeout shouldEqual r
    lfe await timeout shouldEqual l
  }

  test("fold") {
    re.fold(_ => 1, _ => 2).block shouldEqual 2
    le.fold(_ => 1, _ => 2).block shouldEqual 1
    rfe.fold(_ => 1, _ => 2).block shouldEqual 2
    lfe.fold(_ => 1, _ => 2).block shouldEqual 1

    interceptF(re.fold(_ => exception, _ => exception))
    interceptF(rfe.fold(_ => exception, _ => exception))
    interceptF(le.fold(_ => exception, _ => exception))
    interceptF(lfe.fold(_ => exception, _ => exception))
  }

  test("foldRight") {
    re.foldRight(_ + "r").block shouldEqual "r"
    le.foldRight(_ + "r").block shouldEqual "lr"
    rfe.foldRight(_ + "r").block shouldEqual "r"
    lfe.foldRight(_ + "r").block shouldEqual "lr"

    re.foldRight(_ => exception).block shouldEqual "r"
    interceptF(le.foldRight(_ => exception))
    rfe.foldRight(_ => exception).block shouldEqual "r"
    interceptF(lfe.foldRight(_ => exception))
  }

  test("foldLeft") {
    re.foldLeft(_ + "l").block shouldEqual "rl"
    le.foldLeft(_ + "l").block shouldEqual "l"
    rfe.foldLeft(_ + "l").block shouldEqual "rl"
    lfe.foldLeft(_ + "l").block shouldEqual "l"

    interceptF(re.foldLeft(_ => exception))
    le.foldLeft(_ => exception).block shouldEqual "l"
    interceptF(rfe.foldLeft(_ => exception))
    lfe.foldLeft(_ => exception).block shouldEqual "l"
  }

  test("future") {
    re.future.block shouldEqual r
    le.future.block shouldEqual l
    rfe.future.block shouldEqual r
    lfe.future.block shouldEqual l
  }

  test("recover") {
    (re recover { case _: TestException => l }).block shouldEqual r
    (le recover { case _: TestException => l }).block shouldEqual l
    (rfe recover { case _: TestException => l }).block shouldEqual r
    (lfe recover { case _: TestException => l }).block shouldEqual l
    (fe recover { case _: TestException => r }).block shouldEqual r
    (fe recover { case _: TestException => l }).block shouldEqual l
  }

  test("recoverWith") {
    (re recoverWith { case _: TestException => le }).block shouldEqual r
    (le recoverWith { case _: TestException => le }).block shouldEqual l
    (rfe recoverWith { case _: TestException => le }).block shouldEqual r
    (lfe recoverWith { case _: TestException => le }).block shouldEqual l
    (fe recoverWith { case _: TestException => re }).block shouldEqual r
    (fe recoverWith { case _: TestException => le }).block shouldEqual l
    (fe recoverWith { case _: TestException => rfe }).block shouldEqual r
    (fe recoverWith { case _: TestException => lfe }).block shouldEqual l
  }

  test("zipWith") {
    le.zipWith(le)(_ + _).future.futureValue.left.value shouldBe "l"
    le.zipWith(re)(_ + _).future.futureValue.left.value shouldBe "l"
    re.zipWith(le)(_ + _).future.futureValue.left.value shouldBe "l"
    re.zipWith(re)(_ + _).future.futureValue.right.value shouldBe "rr"

    lfe.zipWith(lfe)(_ + _).future.futureValue.left.value shouldBe "l"
    lfe.zipWith(rfe)(_ + _).future.futureValue.left.value shouldBe "l"
    rfe.zipWith(lfe)(_ + _).future.futureValue.left.value shouldBe "l"
    rfe.zipWith(rfe)(_ + _).future.futureValue.right.value shouldBe "rr"

    le.zipWith(lfe)(_ + _).future.futureValue.left.value shouldBe "l"
    lfe.zipWith(le)(_ + _).future.futureValue.left.value shouldBe "l"
    le.zipWith(rfe)(_ + _).future.futureValue.left.value shouldBe "l"
    rfe.zipWith(le)(_ + _).future.futureValue.left.value shouldBe "l"

    rfe.zipWith(re)(_ + _).future.futureValue.right.value shouldBe "rr"
    re.zipWith(rfe)(_ + _).future.futureValue.right.value shouldBe "rr"
  }

  test("orElse") {
    (re orElse re).block shouldEqual r
    (re orElse le).block shouldEqual r
    (re orElse rfe).block shouldEqual r
    (re orElse lfe).block shouldEqual r

    (le orElse re).block shouldEqual r
    (le orElse le).block shouldEqual l
    (le orElse rfe).block shouldEqual r
    (le orElse lfe).block shouldEqual l

    (rfe orElse re).block shouldEqual r
    (rfe orElse le).block shouldEqual r
    (rfe orElse rfe).block shouldEqual r
    (rfe orElse lfe).block shouldEqual r

    (lfe orElse re).block shouldEqual r
    (lfe orElse le).block shouldEqual l
    (lfe orElse rfe).block shouldEqual r
    (lfe orElse lfe).block shouldEqual l
  }

  test("getOrElse") {
    (re getOrElse "l").block shouldEqual "r"
    (le getOrElse "r").block shouldEqual "r"
    (rfe getOrElse "l").block shouldEqual "r"
    (lfe getOrElse "r").block shouldEqual "r"
  }

  test("|") {
    (re | "l").block shouldEqual "r"
    (le | "r").block shouldEqual "r"
    (rfe | "l").block shouldEqual "r"
    (lfe | "r").block shouldEqual "r"
  }

  test("orError") {
    re.orError().block shouldEqual "r"
    rfe.orError().block shouldEqual "r"
    interceptF(le.orError(_ => new TestException))
    interceptF(lfe.orError(_ => new TestException))
  }

  test("onRight") {
    var x = ""

    re onRight { _ => x = "re" }
    x shouldEqual "re"

    rfe onRight { _ => x = "rfe" }
    x shouldEqual "rfe"

    le onRight { _ => x = "le" }
    x shouldEqual "rfe"

    lfe onRight { _ => x = "lfe" }
    x shouldEqual "rfe"
  }

  test("onLeft") {
    var x = ""

    re onLeft { _ => x = "re" }
    x shouldEqual ""

    rfe onLeft { _ => x = "rfe" }
    x shouldEqual ""

    le onLeft { _ => x = "le" }
    x shouldEqual "le"

    lfe onLeft { _ => x = "lfe" }
    x shouldEqual "lfe"
  }

  test("foreach") {
    var x = ""

    re foreach { _ => x = "re" }
    x shouldEqual "re"

    rfe foreach { _ => x = "rfe" }
    x shouldEqual "rfe"

    le foreach { _ => x = "le" }
    x shouldEqual "rfe"

    lfe foreach { _ => x = "lfe" }
    x shouldEqual "rfe"
  }

  test("toRight") {
    re.toRight.block shouldEqual Some("r")
    rfe.toRight.block shouldEqual Some("r")
    le.toRight.block shouldEqual None
    lfe.toRight.block shouldEqual None
  }

  test("toLeft") {
    re.toLeft.block shouldEqual None
    rfe.toLeft.block shouldEqual None
    le.toLeft.block shouldEqual Some("l")
    lfe.toLeft.block shouldEqual Some("l")
  }

  test("exists") {
    re.exists(_ == "r").block shouldEqual true
    re.exists(_ == "l").block shouldEqual false
    rfe.exists(_ == "r").block shouldEqual true
    rfe.exists(_ == "l").block shouldEqual false

    le.exists(_ == "l").block shouldEqual false
    le.exists(_ == "l").block shouldEqual false
    lfe.exists(_ == "l").block shouldEqual false
    lfe.exists(_ == "l").block shouldEqual false
  }

  test("contains") {
    (re contains "r").block shouldEqual true
    (re contains "l").block shouldEqual false
    (rfe contains "r").block shouldEqual true
    (rfe contains "l").block shouldEqual false

    (le contains "l").block shouldEqual false
    (le contains "l").block shouldEqual false
    (lfe contains "l").block shouldEqual false
    (lfe contains "l").block shouldEqual false
  }

  test("forall") {
    re.forall(_ == "r").block shouldEqual true
    re.forall(_ == "l").block shouldEqual false
    rfe.forall(_ == "r").block shouldEqual true
    rfe.forall(_ == "l").block shouldEqual false

    le.forall(_ == "l").block shouldEqual true
    le.forall(_ == "l").block shouldEqual true
    lfe.forall(_ == "l").block shouldEqual true
    lfe.forall(_ == "l").block shouldEqual true
  }

  test("?>>") {
    (re ?>> "ll").block shouldEqual r
    (rfe ?>> "ll").block shouldEqual r
    (le ?>> "ll").block shouldEqual "ll".ko
    (lfe ?>> "ll").block shouldEqual "ll".ko
  }

  test("collect") {
    (re collect { case "r" => "rr" }).block shouldEqual "rr".ok
    (re collect { case "" => "rr" }).block shouldEqual r
    (rfe collect { case "r" => "rr" }).block shouldEqual "rr".ok
    (rfe collect { case "" => "rr" }).block shouldEqual r
    (le collect { case "r" => "rr" }).block shouldEqual l
    (le collect { case "" => "rr" }).block shouldEqual l
    (lfe collect { case "r" => "rr" }).block shouldEqual l
    (lfe collect { case "" => "rr" }).block shouldEqual l
  }

  test("collectWith") {
    (re collectWith { case "r" => "rr".ok.fe }).block shouldEqual "rr".ok
    (re collectWith { case "" => "rr".ok.fe }).block shouldEqual r
    (rfe collectWith { case "r" => "rr".ok.fe }).block shouldEqual "rr".ok
    (rfe collectWith { case "" => "rr".ok.fe }).block shouldEqual r
    (le collectWith { case "r" => "rr".ok.fe }).block shouldEqual l
    (le collectWith { case "" => "rr".ok.fe }).block shouldEqual l
    (lfe collectWith { case "r" => "rr".ok.fe }).block shouldEqual l
    (lfe collectWith { case "" => "rr".ok.fe }).block shouldEqual l

    (re collectWith { case "r" => "ll".ko.fe }).block shouldEqual "ll".ko
    (re collectWith { case "" => "ll".ko.fe }).block shouldEqual r
    (rfe collectWith { case "r" => "ll".ko.fe }).block shouldEqual "ll".ko
    (rfe collectWith { case "" => "ll".ko.fe }).block shouldEqual r
    (le collectWith { case "r" => "ll".ko.fe }).block shouldEqual l
    (le collectWith { case "" => "ll".ko.fe }).block shouldEqual l
    (lfe collectWith { case "r" => "ll".ko.fe }).block shouldEqual l
    (lfe collectWith { case "" => "ll".ko.fe }).block shouldEqual l
  }

  test("list") {
    FutureEither.list(List(
      1.ok.fe,
      2.ok.fe,
      3.ok.fe)).block shouldEqual List(1, 2, 3).ok

    FutureEither.list(List(
      (Future successful 1.ok).fe,
      (Future successful 2.ok).fe,
      (Future successful 3.ok).fe)).block shouldEqual List(1, 2, 3).ok

    FutureEither.list(List(
      1.ok.fe,
      (Future successful 2.ok).fe,
      3.ok.fe)).block shouldEqual List(1, 2, 3).ok

    FutureEither.list(List(
      1.ko.fe,
      2.ok.fe,
      3.ok.fe)).block shouldEqual 1.ko

    FutureEither.list(List(
      1.ko.fe,
      2.ko.fe,
      3.ok.fe)).block shouldEqual 1.ko

    FutureEither.list(List(
      1.ko.fe,
      (Future successful 2.ok).fe,
      (Future successful 3.ok).fe)).block shouldEqual 1.ko

    FutureEither.list(List(
      1.ko.fe,
      (Future successful 2.ko).fe,
      (Future successful 3.ok).fe)).block shouldEqual 1.ko

    FutureEither.list(List(
      1.ko.fe,
      (Future successful 2.ok).fe,
      3.ok.fe)).block shouldEqual 1.ko

    FutureEither.list(List(
      (Future successful 1.ko).fe,
      (Future successful 2.ok).fe,
      3.ok.fe)).block shouldEqual 1.ko
  }

  test("sequence") {
    FutureEither.sequence(Seq(
      1.ok.fe,
      2.ok.fe,
      3.ok.fe)).block shouldEqual Seq(1, 2, 3).ok

    FutureEither.sequence(Seq(
      (Future successful 1.ok).fe,
      (Future successful 2.ok).fe,
      (Future successful 3.ok).fe)).block shouldEqual Seq(1, 2, 3).ok

    FutureEither.sequence(Seq(
      1.ok.fe,
      (Future successful 2.ok).fe,
      3.ok.fe)).block shouldEqual Seq(1, 2, 3).ok

    FutureEither.sequence(Seq(
      1.ko.fe,
      2.ok.fe,
      3.ok.fe)).block shouldEqual 1.ko

    FutureEither.sequence(Seq(
      1.ko.fe,
      2.ko.fe,
      3.ok.fe)).block shouldEqual 1.ko

    FutureEither.sequence(Seq(
      1.ko.fe,
      (Future successful 2.ok).fe,
      (Future successful 3.ok).fe)).block shouldEqual 1.ko

    FutureEither.sequence(Seq(
      1.ko.fe,
      (Future successful 2.ko).fe,
      (Future successful 3.ok).fe)).block shouldEqual 1.ko

    FutureEither.sequence(Seq(
      1.ko.fe,
      (Future successful 2.ok).fe,
      3.ok.fe)).block shouldEqual 1.ko

    FutureEither.sequence(Seq(
      (Future successful 1.ko).fe,
      (Future successful 2.ok).fe,
      3.ok.fe)).block shouldEqual 1.ko
  }

  test("transform") {
    re.transform(_.map(_ => "rr")).block shouldEqual "rr".ok
    rfe.transform(_.map(_ => "rr")).block shouldEqual "rr".ok
    le.transform(_.leftMap(_ => "ll")).block shouldEqual "ll".ko
    lfe.transform(_.leftMap(_ => "ll")).block shouldEqual "ll".ko
  }

  test("transformWith") {
    re.transformWith(_.map(_ => "rr").fe).block shouldEqual "rr".ok
    rfe.transformWith(_.map(_ => "rr").fe).block shouldEqual "rr".ok
    le.transformWith(_.leftMap(_ => "ll").fe).block shouldEqual "ll".ko
    lfe.transformWith(_.leftMap(_ => "ll").fe).block shouldEqual "ll".ko
  }

  test("andThen") {
    var x = 0
    re andThen { case Right(_) => x += 1 }
    rfe andThen { case Right(_) => x += 1 }
    le andThen { case Left(_) => x += 1 }
    lfe andThen { case Left(_) => x += 1 }
    x shouldEqual 4
  }


  test("toString") {
    le.toString shouldEqual "FutureEither(Left(l))"
    re.toString shouldEqual "FutureEither(Right(r))"
    lfe.toString shouldEqual "FutureEither(Left(l))"
    rfe.toString shouldEqual "FutureEither(Right(r))"
    FutureEither(Future.failed(new TestException)).toString shouldEqual "FutureEither(com.evolutiongaming.util.FutureEitherSpec$TestException: test)"
    val promise = Promise[Either[String, String]]
    promise.future.fe.toString shouldEqual "FutureEither(<not completed>)"
  }

  test("fe") {
    (Future successful "x").fe[String].block shouldEqual Right("x")
  }

  test("value") {
    le.value shouldEqual Some(Success("l".ko))
    re.value shouldEqual Some(Success("r".ok))
  }

  private val timeout = 10.seconds

  def exception = throw new TestException

  def interceptFe[L, R](f: => FutureEither[L, R]): Unit = {
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

  implicit class FutureEitherTest[L, R](self: FutureEither[L, R]) {
    def block: Either[L, R] = self.future.block
  }

  implicit class FutureOptionTest[T](self: FutureOption[T]) {
    def block: Option[T] = self.future.block
  }

  implicit def eitherEquality[L, R]: Equality[Either[L, R]] = new Equality[Either[L, R]] {
    def areEqual(a: Either[L, R], b: Any) = a == b
  }
}
