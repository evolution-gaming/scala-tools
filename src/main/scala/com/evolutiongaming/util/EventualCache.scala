package com.evolutiongaming.util

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}

trait EventualCache[K, V] {
  def get(key: K): Option[V]
  def getOrUpdateAwait(key: K)(future: => Future[Option[V]])(implicit timeout: FiniteDuration): Option[V]
  def getOrUpdate(key: K)(future: => Future[Option[V]]): Future[Option[V]]
  def map: collection.Map[K, V]
  def update(key: K, value: V): Unit
  def remove(key: K): Option[V]
}

object EventualCache {
  def apply[K, V]()(implicit ec: ExecutionContext): EventualCache[K, V] = new Impl[K, V]

  private class Impl[K, V](implicit ec: ExecutionContext) extends EventualCache[K, V] {
    private val values = TrieMap.empty[K, V]
    private val promises = TrieMap.empty[K, Promise[Option[V]]]

    def get(key: K) = values.get(key)

    def getOrUpdate(key: K)(future: => Future[Option[V]]): Future[Option[V]] = {
      def fromCache = for {value <- values get key} yield Future successful Some(value)

      def evaluate = {
        val newPromise = Promise[Option[V]]()
        val promise = promises getOrElseUpdate(key, newPromise)
        if (promise == newPromise) future onComplete { value =>
          values get key match {
            case Some(value) => promise trySuccess Some(value)
            case None        =>
              for {value <- value; value <- value} values.getOrElseUpdate(key, value)
              promise tryComplete value
              promises remove key
          }
        }
        promise.future
      }

      fromCache getOrElse evaluate
    }

    def getOrUpdateAwait(key: K)(future: => Future[Option[V]])(implicit timeout: FiniteDuration) = {
      def fromCache = values get key
      def evaluate = {
        val f = getOrUpdate(key)(future)
        val value = for {value <- f.value; value <- value.toOption} yield value
        value getOrElse Await.result(f, timeout)
      }
      fromCache orElse evaluate
    }

    def map = values

    def update(key: K, value: V) = {
      values.update(key, value)
      for {promise <- promises get key} {
        promise trySuccess Some(value)
        promises remove key
      }
    }

    def remove(key: K) = for {
      v <- values remove key
      _ = promises remove key
    } yield v
  }
}
