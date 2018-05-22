package com.evolutiongaming.util

import com.evolutiongaming.concurrent.CurrentThreadExecutionContext

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

sealed trait FutureOption[+T] {

  def future: Future[Option[T]]

  def value: Option[Try[Option[T]]]


  def map[TT](f: T => TT)(implicit ec: ExecutionContext): FutureOption[TT]

  def flatMap[TT](f: T => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT]


  def transform[TT](f: Option[T] => Option[TT])(implicit executor: ExecutionContext): FutureOption[TT]

  def transformWith[TT](f: Option[T] => FutureOption[TT])(implicit executor: ExecutionContext): FutureOption[TT]

  def andThen[U](pf: PartialFunction[Option[T], U])(implicit ec: ExecutionContext): FutureOption[T] = {
    transform { x =>
      try if (pf.isDefinedAt(x)) pf(x)
      catch { case NonFatal(t) => ec reportFailure t }
      x
    }
  }


  def fold[TT](z: => TT)(f: T => TT)(implicit ec: ExecutionContext): Future[TT]

  def orElse[TT >: T](or: => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT]

  def getOrElse[TT >: T](or: => TT)(implicit ec: ExecutionContext): Future[TT] = {
    fold(or)(identity)
  }


  def filter(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T]

  def filterNot(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T] = filter(!f(_))

  def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)


  def isEmpty: Future[Boolean] = fold(true)(_ => false)(CurrentThreadExecutionContext)

  def isDefined: Future[Boolean] = fold(false)(_ => true)(CurrentThreadExecutionContext)

  def nonEmpty: Future[Boolean] = isDefined


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


  def collect[TT >: T](pf: PartialFunction[T, TT])(implicit ec: ExecutionContext): FutureOption[TT]

  def collectWith[TT >: T](pf: PartialFunction[T, FutureOption[TT]])
    (implicit ec: ExecutionContext): FutureOption[TT] = {

    flatMap { x => if (pf isDefinedAt x) pf(x) else FutureOption.empty }
  }


  def recover[TT >: T](pf: PartialFunction[Throwable, TT])
    (implicit ec: ExecutionContext): FutureOption[TT]

  def recoverWith[TT >: T](pf: PartialFunction[Throwable, FutureOption[TT]])
    (implicit ec: ExecutionContext): FutureOption[TT]

  def unit: FutureOption[Unit] = map { _ => {} }(CurrentThreadExecutionContext)


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


  private case class HasFuture[+T](self: Future[Option[T]]) extends FutureOption[T] {

    def future: Future[Option[T]] = self

    def value: Option[Try[Option[T]]] = self.value


    def map[TT](f: T => TT)(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasFuture(self map { _ map f })
    }

    def flatMap[TT](f: T => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasFuture(self.flatMap {
        case None    => Future successful None
        case Some(x) => f(x).future
      })
    }

    def transform[TT](f: Option[T] => Option[TT])(implicit executor: ExecutionContext): FutureOption[TT] = {
      HasFuture(self.map(f))
    }

    def transformWith[TT](f: Option[T] => FutureOption[TT])(implicit executor: ExecutionContext): FutureOption[TT] = {
      HasFuture(self.flatMap(f(_).future))
    }


    def fold[TT](z: => TT)(f: T => TT)(implicit ec: ExecutionContext): Future[TT] = {
      self map { _.fold(z)(f) }
    }

    def orElse[TT >: T](or: => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasFuture(self.flatMap {
        case None    => or.future
        case Some(x) => Future successful Some(x)
      })
    }

    def filter(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T] = {
      HasFuture(self map { _.filter(f) })
    }

    def foreach[TT](f: T => TT)(implicit ec: ExecutionContext): Unit = {
      for {x <- self; x <- x} f(x)
    }

    def await(timeout: Duration): Option[T] = Await.result(self, timeout)

    def toRight[L](l: => L)(implicit ec: ExecutionContext): FutureEither[L, T] = {
      FutureEither(self map { _.toRight(l) })
    }

    def toLeft[R](r: => R)(implicit ec: ExecutionContext): FutureEither[T, R] = {
      FutureEither(self map { _.toLeft(r) })
    }

    def collect[TT >: T](pf: PartialFunction[T, TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasFuture(self map { _ collect pf })
    }

    def recover[TT >: T](pf: PartialFunction[Throwable, TT])
      (implicit ec: ExecutionContext): FutureOption[TT] = {

      HasFuture(future recover { case x if pf isDefinedAt x => Some(pf(x)) })
    }

    def recoverWith[TT >: T](pf: PartialFunction[Throwable, FutureOption[TT]])
      (implicit ec: ExecutionContext): FutureOption[TT] = {

      HasFuture(future recoverWith { case x if pf isDefinedAt x => pf(x).future })
    }

    override def toString = self.value match {
      case Some(Success(value)) => s"FutureOption($value)"
      case Some(Failure(value)) => s"FutureOption($value)"
      case None                 => "FutureOption(<not completed>)"
    }
  }


  private case class HasOption[+T](self: Option[T]) extends FutureOption[T] {

    def future: Future[Option[T]] = Future successful self

    def value: Option[Try[Option[T]]] = Some(Success(self))


    def map[TT](f: T => TT)(implicit ec: ExecutionContext): FutureOption[TT] = {
      safe(HasOption(self map f))
    }

    def flatMap[TT](f: T => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      self match {
        case None    => HasOption(None)
        case Some(x) => safe(f(x))
      }
    }


    def transform[TT](f: Option[T] => Option[TT])(implicit executor: ExecutionContext): FutureOption[TT] = {
      safe(HasOption(f(self)))
    }

    def transformWith[TT](f: Option[T] => FutureOption[TT])(implicit executor: ExecutionContext): FutureOption[TT] = {
      safe(f(self))
    }


    def fold[TT](z: => TT)(f: T => TT)(implicit ec: ExecutionContext): Future[TT] = {
      safe(Future successful self.fold(z)(f))
    }

    def orElse[TT >: T](or: => FutureOption[TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      self match {
        case Some(_) => this
        case None    => safe(or)
      }
    }

    def filter(f: T => Boolean)(implicit ec: ExecutionContext): FutureOption[T] = {
      HasOption(self.filter(f))
    }

    def foreach[TT](f: T => TT)(implicit ec: ExecutionContext): Unit = {
      self foreach f
    }

    def await(timeout: Duration): Option[T] = self

    def toRight[L](l: => L)(implicit ec: ExecutionContext): FutureEither[L, T] = {
      FutureEither(self.toRight(l))
    }

    def toLeft[R](r: => R)(implicit ec: ExecutionContext): FutureEither[T, R] = {
      FutureEither(self.toLeft(r))
    }

    def collect[TT >: T](pf: PartialFunction[T, TT])(implicit ec: ExecutionContext): FutureOption[TT] = {
      HasOption(self collect pf)
    }

    def recover[TT >: T](pf: PartialFunction[Throwable, TT])
      (implicit ec: ExecutionContext): FutureOption[TT] = this

    def recoverWith[TT >: T](pf: PartialFunction[Throwable, FutureOption[TT]])
      (implicit ec: ExecutionContext): FutureOption[TT] = this


    private def safe[TT](f: => Future[TT]): Future[TT] = {
      try f catch { case NonFatal(x) => Future failed x }
    }

    private def safe[TT](f: => FutureOption[TT]): FutureOption[TT] = {
      try f catch { case NonFatal(x) => HasFuture(Future failed x) }
    }

    override def toString = s"FutureOption($self)"
  }
}
