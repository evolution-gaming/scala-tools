package com.evolutiongaming.util

import com.evolutiongaming.util.Validation._

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds


trait FutureEitherSequence {

  def sequence[L, R, M[X] <: TraversableOnce[X]](in: M[FutureEither[L, R]])
    (implicit ec: ExecutionContext, cbf: CanBuildFrom[M[FutureEither[L, R]], R, M[R]]): FutureEither[L, M[R]] = {

    in.foldLeft(Future.successful(cbf(in)).fe[L]) {
      case (acc, next: FutureEither[L, R]) => acc.zipWith(next)(_ += _)
    }.map(_.result())
  }
}