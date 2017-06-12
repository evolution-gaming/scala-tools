package com.evolutiongaming.util

import com.evolutiongaming.concurrent.CurrentThreadExecutionContext
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.NoStackTrace

class RunOneAtATimeSpec extends WordSpec with Matchers {
  "RunOneAtATime" should {
    "work for Future.successful" in new Scope {
      val future = runOneAtATime("0")(successful)
      Await.result(future, timeout) shouldEqual 0
    }

    "not run many futures concurrently" in new Scope {
      runOneAtATime("0")(promise.future)
      val future = runOneAtATime("0")(failed)
      promise.success(0)
      Await.result(future, timeout) shouldEqual 0
    }

    "not run many futures concurrently even on failures" in new Scope {
      runOneAtATime("0")(promise.future)
      val future = runOneAtATime("0")(successful)
      promise.failure(TestException)
      the[TestException.type] thrownBy Await.result(future, timeout)
    }

    "not leak" in new Scope {
      val future = runOneAtATime("0")(successful)(CurrentThreadExecutionContext)
      Await.result(future, timeout) shouldEqual 0
      Await.result(runOneAtATime("0")(Future.successful(1)), timeout) shouldEqual 1
    }
  }

  private trait Scope {
    val timeout = 3.seconds
    val promise = Promise[Int]
    val runOneAtATime = new RunOneAtATime[String, Int]()
    val successful = Future.successful(0)
    val failed = Future.failed(TestException)

    def async[T](f: => T): Future[T] = {
      val future = Future(f)
      the[TimeoutException] thrownBy Await.ready(future, 300.millis)
      future
    }
  }

  private object TestException extends RuntimeException with NoStackTrace
}
