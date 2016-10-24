package com.evolutiongaming.util

import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.util.control.NoStackTrace

class EventualCacheSpec extends WordSpec with Matchers {
  implicit val timeout = 3.seconds

  "EventualCache" when {

    "get" should {
      "return None" in new Scope {
        cache.get(0) shouldEqual None
      }

      "return Some" in new Scope {
        cache.update(0, "0")
        cache.get(0) shouldEqual Some("0")
      }
    }

    "getOrUpdate" should {
      "return value if found" in new Scope {
        cache.update(0, "0")
        val future = cache.getOrUpdate(0) {throw TestException}
        Await.result(future, timeout) shouldEqual Some("0")
      }

      "return evaluated some value" in new Scope {
        val future = cache.getOrUpdate(0)(successfulSome)
        Await.result(future, timeout) shouldEqual Some("0")
      }

      "return evaluated some and save" in new Scope {
        Await.result(cache.getOrUpdate(0)(successfulSome), timeout) shouldEqual Some("0")
        cache.get(0) shouldEqual Some("0")
        Await.result(cache.getOrUpdate(0)(Future.successful(Some("1"))), timeout) shouldEqual Some("0")
      }

      "return evaluated none value" in new Scope {
        val future = cache.getOrUpdate(0)(successfulNone)
        Await.result(future, timeout) shouldEqual None
      }

      "return evaluated none and not save" in new Scope {
        Await.result(cache.getOrUpdate(0)(successfulNone), timeout) shouldEqual None
        cache.get(0) shouldEqual None
        Await.result(cache.getOrUpdate(0)(successfulSome), timeout) shouldEqual Some("0")
      }

      "throw exception if evaluation timed out" in new Scope {
        the[TimeoutException] thrownBy Await.result(cache.getOrUpdate(0)(Future.failed(new TimeoutException)), timeout)
        cache.get(0) shouldEqual None
      }

      "return Some if updated within failed evaluation" in new Scope {
        val future = cache.getOrUpdate(0)(promise.future)
        cache.update(0, "0")
        promise.failure(TestException)
        Await.result(future, timeout) shouldEqual Some("0")
      }

      "not evaluate values concurrently for the same key" in new Scope {
        val future = cache.getOrUpdate(0)(promise.future)
        val future2 = cache.getOrUpdate(0)(Future.failed(TestException))

        promise.success(Some("0"))

        Await.result(future2, timeout) shouldEqual Some("0")
      }

      "throw exception if evaluation failed" in new Scope {
        the[TestException.type] thrownBy Await.result(cache.getOrUpdate(0)(Future.failed(TestException)), timeout)
      }

      "return updated value before future completed" in new Scope {
        val future = cache.getOrUpdate(0)(promise.future)
        cache.update(0, "1")
        Await.result(future, timeout) shouldEqual Some("1")
      }

      "not override newer value" in new Scope {
        val future = cache.getOrUpdate(0)(promise.future)
        cache.update(0, "1")
        promise.success(Some("0"))

        Await.result(future, timeout) shouldEqual Some("1")
        cache.get(0) shouldEqual Some("1")
      }
    }

    "getOrUpdateAwait" should {
      "return value if found" in new Scope {
        cache.update(0, "0")
        cache.getOrUpdateAwait(0) {throw TestException} shouldEqual Some("0")
      }

      "return evaluated some value" in new Scope {
        cache.getOrUpdateAwait(0)(successfulSome) shouldEqual Some("0")
      }

      "return evaluated some and save" in new Scope {
        cache.getOrUpdateAwait(0)(successfulSome) shouldEqual Some("0")
        cache.get(0) shouldEqual Some("0")
        cache.getOrUpdateAwait(0)(Future.successful(Some("1"))) shouldEqual Some("0")
      }

      "return evaluated none value" in new Scope {
        cache.getOrUpdateAwait(0)(successfulNone) shouldEqual None
      }

      "return evaluated none and not save" in new Scope {
        cache.getOrUpdateAwait(0)(successfulNone) shouldEqual None
        cache.get(0) shouldEqual None
        cache.getOrUpdateAwait(0)(successfulSome) shouldEqual Some("0")
      }

      "throw exception if evaluation timed out" in new Scope {
        the[TimeoutException] thrownBy cache.getOrUpdateAwait(0)(Future.failed(new TimeoutException))
        cache.get(0) shouldEqual None
      }

      "return Some if updated within failed evaluation" in new Scope {
        val future = async {
          cache.getOrUpdateAwait(0)(promise.future)
        }
        cache.update(0, "0")
        promise.failure(TestException)
        Await.result(future, timeout) shouldEqual Some("0")
      }

      "not evaluate values concurrently for the same key" in new Scope {
        val future = async {
          cache.getOrUpdateAwait(0)(promise.future)
        }

        val future2 = async {
          cache.getOrUpdateAwait(0)(Future.failed(TestException))
        }

        promise.success(Some("0"))

        Await.result(future2, timeout) shouldEqual Some("0")
      }

      "throw exception if evaluation failed" in new Scope {
        the[TestException.type] thrownBy cache.getOrUpdateAwait(0)(Future.failed(TestException))
      }

      "return updated value before future completed" in new Scope {
        val future = async {
          cache.getOrUpdateAwait(0)(promise.future)
        }
        cache.update(0, "1")
        Await.result(future, timeout) shouldEqual Some("1")
      }

      "not override newer value" in new Scope {
        val future = async {
          cache.getOrUpdateAwait(0)(promise.future)
        }
        cache.update(0, "1")
        promise.success(Some("0"))

        Await.result(future, timeout) shouldEqual Some("1")
        cache.get(0) shouldEqual Some("1")
      }
    }

    "map" should {
      "return map with all entries" in new Scope {
        cache.update(0, "0")
        cache.update(1, "1")
        cache.map shouldEqual Map(0 -> "0", 1 -> "1")
      }
    }

    "update" should {
      "add entry" in new Scope {
        cache.get(0) shouldEqual None
        cache.update(0, "0")
        cache.get(0) shouldEqual Some("0")
      }

      "update entry" in new Scope {
        cache.update(0, "0")
        cache.update(0, "1")
        cache.get(0) shouldEqual Some("1")
      }
    }

    "remove" should {
      "remove entry" in new Scope {
        cache.remove(0) shouldEqual None
      }

      "return None if entry not found" in new Scope {
        cache.update(0, "0")
        cache.remove(0) shouldEqual Some("0")
        cache.remove(0) shouldEqual None
      }
    }
  }

  private trait Scope {
    val cache = EventualCache[Int, String]()
    val promise = Promise[Option[String]]
    val successfulSome = Future.successful(Some("0"))
    val successfulNone = Future.successful(None)
    val failed = Future.failed(TestException)

    def async[T](f: => T): Future[T] = {
      val future = Future(f)
      the[TimeoutException] thrownBy Await.ready(future, 300.millis)
      future
    }

  }
  private object TestException extends RuntimeException with NoStackTrace
}
