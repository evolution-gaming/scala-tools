package com.evolutiongaming.util

import com.evolutiongaming.util.Validation._

import scala.collection.BuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds


trait FutureEitherSequence {

  def sequence[L, R, M[X] <: IterableOnce[X]](in: M[FutureEither[L, R]])
    (implicit ec: ExecutionContext, cbf: BuildFrom[M[FutureEither[L, R]], R, M[R]]): FutureEither[L, M[R]] = {

    in.iterator.foldLeft(Future.successful(cbf.newBuilder(in)).fe[L]) {
      case (acc, next: FutureEither[L, R]) => acc.zipWith(next)(_ += _)
    }.map(_.result())
  }
}