package com.evolutiongaming.util

import scala.collection.BuildFrom
import scala.concurrent.{ExecutionContext, Future}

trait FutureOptionSequence {

  def sequence[A, M[X] <: IterableOnce[X]](in: M[FutureOption[A]])
    (implicit ec: ExecutionContext, cbf: BuildFrom[M[FutureOption[A]], A, M[A]]): Future[M[A]] = {

    traverse(in)(identity)
  }

  def traverse[A, B, M[X] <: IterableOnce[X]](in: M[A])(f: A => FutureOption[B])
    (implicit ec: ExecutionContext, cbf: BuildFrom[M[A], B, M[B]]): Future[M[B]] = {

    in.iterator.foldLeft(Future successful cbf.newBuilder(in)) { (builder, x) =>
      for {
        prev <- builder
        x <- f(x).future
      } yield prev ++= x
    } map { _.result }
  }
}