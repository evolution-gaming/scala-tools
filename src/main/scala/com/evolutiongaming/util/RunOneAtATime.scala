package com.evolutiongaming.util

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Yaroslav Klymko
  */
class RunOneAtATime[A, B] {
  private val map: TrieMap[A, Future[B]] = TrieMap.empty

  def apply(key: A)(future: => Future[B])(implicit ec: ExecutionContext): Future[B] = {
    var updated = false
    val result = map.getOrElseUpdate(key, {updated = true; future})
    if (updated) result onComplete { _ => map remove key }
    result
  }
}