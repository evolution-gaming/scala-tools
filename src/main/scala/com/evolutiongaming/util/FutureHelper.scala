package com.evolutiongaming.util

import com.evolutiongaming.concurrent.CurrentThreadExecutionContext

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object FutureHelper {
  private val futureUnit = Future.successful(())

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


  implicit class FutureObjOps(val self: Future.type) extends AnyVal {
    def unit: Future[Unit] = futureUnit
  }

  implicit class FutureOps[T](val self: Future[T]) extends AnyVal {
    def unit: Future[Unit] = self.map { _ => {} }(CurrentThreadExecutionContext)
  }
}