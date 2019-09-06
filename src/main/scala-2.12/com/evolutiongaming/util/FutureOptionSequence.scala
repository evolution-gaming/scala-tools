package com.evolutiongaming.util

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}

trait FutureOptionSequence {

  def sequence[A, M[X] <: TraversableOnce[X]](in: M[FutureOption[A]])
    (implicit ec: ExecutionContext, cbf: CanBuildFrom[M[FutureOption[A]], A, M[A]]): Future[M[A]] = {

    traverse(in)(identity)
  }

  def traverse[A, B, M[X] <: TraversableOnce[X]](in: M[A])(f: A => FutureOption[B])
    (implicit ec: ExecutionContext, cbf: CanBuildFrom[M[A], B, M[B]]): Future[M[B]] = {

    in.foldLeft(Future successful cbf(in)) { (builder, x) =>
      for {
        prev <- builder
        x <- f(x).future
      } yield prev ++= x
    } map { _.result }
  }
}