package com.evolutiongaming.util

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal

sealed trait FutureOption[+T] {

  def future: Future[Option[T]]


  def map[TT](f: T => TT)(implicit ec: ExecutionContext): FutureOption[TT]

  def flatMap[TT](f: T => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT]


  def fold[TT](z: => TT)(f: T => TT)(implicit ec: ExecutionContext): Future[TT]

  def orElse[TT >: T](or: => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT]

  def getOrElse[TT >: T](or: => TT)(implicit ec: ExecutionContext): Future[TT] = {
    fold(or)(identity)
  }


  def filter(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T]

  def filterNot(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T] = filter(!f(_))

  def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)


  def isEmpty(implicit ec: ExecutionContext): Future[Boolean] = fold(true)(_ => false)

  def isDefined(implicit ec: ExecutionContext): Future[Boolean] = fold(false)(_ => true)

  def nonEmpty(implicit ec: ExecutionContext): Future[Boolean] = isDefined


  def |[TT >: T](or: => TT)(implicit ec: ExecutionContext): Future[TT] = getOrElse(or)

  def orError(error: => Throwable = new NoSuchElementException)(implicit ec: ExecutionContext): Future[T] = {
    getOrElse(throw error)
  }

  def foreach[TT](f: T => TT)(implicit ec: ExecutionContext): Unit


  def exists(f: T => Boolean)(implicit ec: ExecutionContext): Future[Boolean] = fold(false)(f)

  def contains[TT >: T](x: TT)(implicit ec: ExecutionContext): Future[Boolean] = exists(_ == x)

  def forall(f: T => Boolean)(implicit ec: ExecutionContext): Future[Boolean] = fold(true)(f)


  def await(timeout: Duration): Option[T]


  def toRight[L](l: => L)(implicit ec: ExecutionContext): FutureEither[L, T]

  def toLeft[R](r: => R)(implicit ec: ExecutionContext): FutureEither[T, R]

  def ?>>[L](l: => L)(implicit ec: ExecutionContext): FutureEither[L, T] = toRight(l)


  def collect[TT >: T](pf: PartialFunction[T, TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
    map { x => if (pf isDefinedAt x) pf(x) else x }
  }

  def collectWith[TT >: T](pf: PartialFunction[T, FutureOption[TT]])
    (implicit ec: ExecutionContext): FutureOption[TT] = {

    flatMap { x => if (pf isDefinedAt x) pf(x) else FutureOption(x) }
  }

  
  def recover[TT >: T](pf: PartialFunction[Throwable, Option[TT]])
    (implicit ec: ExecutionContext): FutureOption[TT]

  def recoverWith[TT >: T](pf: PartialFunction[Throwable, FutureOption[TT]])
    (implicit ec: ExecutionContext): FutureOption[TT]


  class WithFilter(p: T => Boolean) {
    private def self = FutureOption.this

    def map[TT](f: T => TT)(implicit ec: ExecutionContext): FutureOption[TT] = {
      self filter p map f
    }

    def flatMap[TT](f: T => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      self filter p flatMap f
    }

    def foreach[TT](f: T => TT)(implicit ec: ExecutionContext): Unit = {
      self filter p foreach f
    }
    
    def withFilter(q: T => Boolean): WithFilter = {
      new WithFilter(x => p(x) && q(x))
    }
  }
}

object FutureOption {

  def apply[T](x: T): FutureOption[T] = FutureOption(Some(x))

  def apply[T](x: Option[T]): FutureOption[T] = HasOption(x)

  def apply[T](x: Future[Option[T]]): FutureOption[T] = HasFuture(x)

  def empty[T]: FutureOption[T] = HasOption(None)


  private case class HasFuture[+T](value: Future[Option[T]]) extends FutureOption[T] {

    def future: Future[Option[T]] = value

    def map[TT](f: T => TT)(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasFuture(value map { _ map f })
    }

    def flatMap[TT](f: T => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasFuture(for {
        x <- value
        x <- x match {
          case None    => Future successful None
          case Some(x) => f(x).future
        }
      } yield x)
    }

    def fold[TT](z: => TT)(f: T => TT)(implicit ec: ExecutionContext): Future[TT] = {
      value map { _.fold(z)(f)}
    }

    def orElse[TT >: T](or: => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasFuture(for {
        x <- value
        x <- x match {
          case None    => or.future
          case Some(x) => Future successful Some(x)
        }
      } yield x)
    }

    def filter(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T] = {
      HasFuture(value map { _.filter(f) })
    }

    def foreach[TT](f: T => TT)(implicit ec: ExecutionContext): Unit = {
      for {x <- value; x <- x} f(x)
    }

    def await(timeout: Duration): Option[T] = Await.result(value, timeout)

    def toRight[L](l: => L)(implicit ec: ExecutionContext): FutureEither[L, T] = {
      FutureEither(value map {_.toRight(l)})
    }

    def toLeft[R](r: => R)(implicit ec: ExecutionContext): FutureEither[T, R] = {
      FutureEither(value map {_.toLeft(r)})
    }

    def recover[TT >: T](pf: PartialFunction[Throwable, Option[TT]])
      (implicit ec: ExecutionContext): FutureOption[TT] = {

      HasFuture(future recover { case x if pf isDefinedAt x => pf(x) })
    }

    def recoverWith[TT >: T](pf: PartialFunction[Throwable, FutureOption[TT]])
      (implicit ec: ExecutionContext): FutureOption[TT] = {

      HasFuture(future recoverWith { case x if pf isDefinedAt x => pf(x).future })
    }

    override def toString = value.value match {
      case Some(value) => s"FutureOption($value)"
      case None        => "FutureOption(<not completed>)"
    }
  }


  private case class HasOption[+T](value: Option[T]) extends FutureOption[T] {

    def future: Future[Option[T]] = Future successful value

    def map[TT](f: T => TT)(implicit ec: ExecutionContext): FutureOption[TT] = {
      safe(HasOption(value map f))
    }

    def flatMap[TT](f: T => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      value match {
        case None    => HasOption(None)
        case Some(x) => safe(f(x))
      }
    }

    def fold[TT](z: => TT)(f: T => TT)(implicit ec: ExecutionContext): Future[TT] = {
      safe(Future successful value.fold(z)(f))
    }

    def orElse[TT >: T](or: => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      value match {
        case Some(_) => this
        case None    => safe(or)
      }
    }

    def filter(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T] = {
      HasOption(value.filter(f))
    }

    def foreach[TT](f: T => TT)(implicit ec: ExecutionContext): Unit = {
      value foreach f
    }

    def await(timeout: Duration): Option[T] = value

    def toRight[L](l: => L)(implicit ec: ExecutionContext): FutureEither[L, T] = {
      FutureEither(value.toRight(l))
    }

    def toLeft[R](r: => R)(implicit ec: ExecutionContext): FutureEither[T, R] = {
      FutureEither(value.toLeft(r))
    }

    def recover[TT >: T](pf: PartialFunction[Throwable, Option[TT]])
      (implicit ec: ExecutionContext): FutureOption[TT] = this

    def recoverWith[TT >: T](pf: PartialFunction[Throwable, FutureOption[TT]])
      (implicit ec: ExecutionContext): FutureOption[TT] = this


    private def safe[TT](f: => Future[TT]): Future[TT] = {
      try f catch { case NonFatal(x) => Future failed x }
    }

    private def safe[TT](f: => FutureOption[TT]): FutureOption[TT] = {
      try f catch { case NonFatal(x) => HasFuture(Future failed x) }
    }

    override def toString = s"FutureOption($value)"
  }
}
