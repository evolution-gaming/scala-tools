package com.evolutiongaming.util

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object FutureHelper {

  def traverseSequentially[A, B, M[X] <: TraversableOnce[X]](in: M[A])(f: A => Future[B])
    (implicit ec: ExecutionContext, cbf: CanBuildFrom[M[A], B, M[B]]): Future[M[B]] = {

    val builder = cbf()
    builder sizeHint in.size

    in.foldLeft(Future successful builder) { (prev, next) =>
      for {
        prev <- prev
        next <- f(next)
      } yield prev += next
    } map { builder => builder.result }
  }
}
