package com.evolutiongaming.util

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.util.{Either, Left, Right}


trait ValidationIterableOps {

  implicit final def validationIterableOps[A, Repr](self: Iterable[A]): IterableOps[A, Repr] =
    new IterableOps[A, Repr](self)
}

final class IterableOps[A, Repr](val self: Iterable[A]) extends AnyVal {

  def ?>>[B](left: => B): Either[B, Iterable[A]] = if (self.isEmpty) Left(left) else Right(self)

  /**
    * Checks that all elements in `self` are valid by applying the `test` function to each.
    * If all are valid, returns a [[scala.util.Right]] with a collection of whatever "good"
    * values `test` returns. Otherwise returns the first [[scala.util.Left]] encountered.
    *
    * @param test function to apply to each element
    * @tparam L type of error the `test` function returns
    * @tparam R type of the good value the `test` function produces
    */
  def allValid[L, R, That](test: A => Either[L, R])(implicit cbf: CanBuildFrom[_, R, That]): Either[L, That] = {
    val b = cbf()

    @tailrec
    def loop(it: Iterable[A]): Either[L, That] = {
      it.headOption match {
        case Some(a) => test(a) match {
          case Left(l)  => Left(l)
          case Right(r) => b += r; loop(it.tail)
        }
        case None    => Right(b.result())
      }
    }
    loop(self)
  }
}