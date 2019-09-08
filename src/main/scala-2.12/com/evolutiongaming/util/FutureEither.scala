package com.evolutiongaming.util

import com.evolutiongaming.concurrent.CurrentThreadExecutionContext
import com.evolutiongaming.util.Validation._

import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.higherKinds
import scala.util.control.NonFatal
import scala.util.{Either, Failure, Success, Try}

sealed trait FutureEither[+L, +R] {

  def future: Future[Either[L, R]]

  def value: Option[Try[Either[L, R]]]


  def map[RR](f: R => RR)(implicit ec: ExecutionContext): FutureEither[L, RR]

  def flatMap[LL >: L, RR](f: R => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR]


  def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): FutureEither[LL, R]

  def leftFlatMap[LL, RR >: R](f: L => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR]


  def transform[LL, RR](f: Either[L, R] => Either[LL, RR])(implicit executor: ExecutionContext): FutureEither[LL, RR]

  def transformWith[LL, RR](f: Either[L, R] => FutureEither[LL, RR])(implicit executor: ExecutionContext): FutureEither[LL, RR]

  def andThen[U](pf: PartialFunction[Either[L, R], U])(implicit ec: ExecutionContext): FutureEither[L, R] = {
    transform { x =>
      try if (pf.isDefinedAt(x)) pf(x)
      catch { case NonFatal(t) => ec reportFailure t }
      x
    }
  }

  def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T]

  def foldRight[RR >: R](f: L => RR)(implicit ec: ExecutionContext): Future[RR] = {
    fold(f, identity)
  }

  def foldLeft[LL >: L](f: R => LL)(implicit ec: ExecutionContext): Future[LL] = {
    fold(identity, f)
  }


  def orElse[LL, RR >: R](or: => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR] = {
    leftFlatMap(_ => or)
  }

  def getOrElse[RR >: R](or: => RR)(implicit ec: ExecutionContext): Future[RR] = {
    foldRight(_ => or)
  }

  def |[RR >: R](or: => RR)(implicit ec: ExecutionContext): Future[RR] = getOrElse(or)

  def orError(toError: L => Throwable = x => new LeftException(x.toString))(implicit ec: ExecutionContext): Future[R] = {
    foldRight { l => throw toError(l) }
  }


  def onRight[U](f: R => U)(implicit ec: ExecutionContext): Unit

  def onLeft[U](f: L => U)(implicit ec: ExecutionContext): Unit

  def foreach[U](f: R => U)(implicit ec: ExecutionContext): Unit = onRight(f)


  def toRight: FutureOption[R]

  def toLeft: FutureOption[L]


  def exists(f: R => Boolean)(implicit ec: ExecutionContext): Future[Boolean] = fold(_ => false, f)

  def contains[RR >: R](x: RR)(implicit ec: ExecutionContext): Future[Boolean] = exists(_ == x)

  def forall(f: R => Boolean)(implicit ec: ExecutionContext): Future[Boolean] = fold(_ => true, f)


  def ?>>[LL](x: => LL)(implicit ec: ExecutionContext): FutureEither[LL, R] = leftMap { _ => x }


  def fallbackTo[LL >: L, BB >: R](that: => FutureEither[LL, BB])(implicit ec: ExecutionContext): FutureEither[LL, BB] = {
    leftFlatMap { l => that leftMap { _ => l } }
  }

  def await(timeout: Duration): Either[L, R]


  def collect[RR >: R](pf: PartialFunction[R, RR])(implicit ec: ExecutionContext): FutureEither[L, RR] = {
    map { x => if (pf isDefinedAt x) pf(x) else x }
  }

  def collectWith[LL >: L, RR >: R](pf: PartialFunction[R, FutureEither[LL, RR]])
    (implicit ec: ExecutionContext): FutureEither[LL, RR] = {

    flatMap { x => if (pf isDefinedAt x) pf(x) else FutureEither[LL, RR](x) }
  }

  def recover[LL >: L, RR >: R](pf: PartialFunction[Throwable, Either[LL, RR]])
    (implicit ec: ExecutionContext): FutureEither[LL, RR]

  def recoverWith[LL >: L, RR >: R](pf: PartialFunction[Throwable, FutureEither[LL, RR]])
    (implicit ec: ExecutionContext): FutureEither[LL, RR]

  def zipWith[LL >: L, RR, RRR](that: FutureEither[LL, RR])(f: (R, RR) => RRR)
    (implicit executionContext: ExecutionContext): FutureEither[LL, RRR] = {

    val result = for {
      er <- future
      err <- that.future
    } yield for {
      r <- er
      rr <- err
    } yield f(r, rr)
    result.fe
  }

  def unit: FutureEither[L, Unit] = map { _ => {} }(CurrentThreadExecutionContext)
}

object FutureEither {

  def apply[L, R](x: Either[L, R]): FutureEither[L, R] = HasEither[L, R](x)

  def apply[L, R](x: R): FutureEither[L, R] = HasEither[L, R](x.ok)

  def apply[L, R](x: Future[Either[L, R]]): FutureEither[L, R] = HasFuture[L, R](x)

  def list[L, R](xs: List[FutureEither[L, R]])
    (implicit ec: ExecutionContext): FutureEither[L, List[R]] = sequence(xs)

  def sequence[L, R, M[X] <: TraversableOnce[X]](in: M[FutureEither[L, R]])
    (implicit ec: ExecutionContext, cbf: CanBuildFrom[M[FutureEither[L, R]], R, M[R]]): FutureEither[L, M[R]] = {

    in.foldLeft(Future.successful(cbf(in)).fe[L]) {
      case (acc, next: FutureEither[L, R]) => acc.zipWith(next)(_ += _)
    }.map(_.result())
  }

  private val completedUnit: FutureEither[Nothing, Unit] = FutureEither(().ok)

  def unit[T]: FutureEither[T, Unit] = completedUnit

  private final case class HasFuture[+L, +R](self: Future[Either[L, R]]) extends FutureEither[L, R] {

    def future: Future[Either[L, R]] = self

    def value: Option[Try[Either[L, R]]] = self.value


    def map[RR](f: R => RR)(implicit ec: ExecutionContext): FutureEither[L, RR] = {
      HasFuture(self map { _ map f })
    }

    def flatMap[LL >: L, RR](f: R => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR] = {
      HasFuture(self.flatMap {
        case Left(x)  => Future successful x.ko
        case Right(x) => f(x).future
      })
    }


    def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): FutureEither[LL, R] = {
      HasFuture(self map { _ leftMap f })
    }

    def leftFlatMap[LL, RR >: R](f: (L) => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR] = {
      HasFuture(self.flatMap {
        case Left(x)  => f(x).future
        case Right(x) => Future successful x.ok
      })
    }


    def transform[LL, RR](f: Either[L, R] => Either[LL, RR])(implicit executor: ExecutionContext): FutureEither[LL, RR] = {
      HasFuture(self.map(f))
    }

    def transformWith[LL, RR](f: Either[L, R] => FutureEither[LL, RR])(implicit executor: ExecutionContext): FutureEither[LL, RR] = {
      HasFuture(self.flatMap(f(_).future))
    }


    def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T] = {
      self map { _.fold(l, r) }
    }


    def onRight[U](f: R => U)(implicit ec: ExecutionContext): Unit = {
      self foreach { _ onRight f }
    }

    def onLeft[U](f: L => U)(implicit ec: ExecutionContext): Unit = {
      self foreach { _ onLeft f }
    }


    def toRight: FutureOption[R] = {
      FutureOption(self.map { _.toRight }(CurrentThreadExecutionContext))
    }

    def toLeft: FutureOption[L] = {
      FutureOption(self.map { _.toLeft }(CurrentThreadExecutionContext))
    }


    def await(timeout: Duration): Either[L, R] = Await.result(future, timeout)


    def recover[LL >: L, RR >: R](pf: PartialFunction[Throwable, Either[LL, RR]])
      (implicit ec: ExecutionContext): FutureEither[LL, RR] = {

      HasFuture[LL, RR](future recover { case x if pf isDefinedAt x => pf(x) })
    }

    def recoverWith[LL >: L, RR >: R](pf: PartialFunction[Throwable, FutureEither[LL, RR]])
      (implicit ec: ExecutionContext): FutureEither[LL, RR] = {

      HasFuture[LL, RR](future recoverWith { case x if pf isDefinedAt x => pf(x).future })
    }


    override def toString = self.value match {
      case Some(Success(value)) => s"FutureEither($value)"
      case Some(Failure(value)) => s"FutureEither($value)"
      case None                 => "FutureEither(<not completed>)"
    }
  }


  private final case class HasEither[+L, +R](self: Either[L, R]) extends FutureEither[L, R] {

    def future: Future[Either[L, R]] = Future successful self

    def value: Option[Try[Either[L, R]]] = Some(Success(self))


    def map[RR](f: R => RR)(implicit ec: ExecutionContext) = {
      safe(HasEither(self map f))
    }

    def flatMap[LL >: L, RR](f: R => FutureEither[LL, RR])(implicit ec: ExecutionContext) = {
      self match {
        case Left(x)  => HasEither(x.ko)
        case Right(x) => safe(f(x))
      }
    }


    def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): FutureEither[LL, R] = {
      safe(HasEither(self leftMap f))
    }

    def leftFlatMap[LL, RR >: R](f: (L) => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR] = {
      self match {
        case Left(x)  => safe(f(x))
        case Right(x) => HasEither(x.ok)
      }
    }


    def transform[LL, RR](f: Either[L, R] => Either[LL, RR])(implicit executor: ExecutionContext): FutureEither[LL, RR] = {
      safe(HasEither(f(self)))
    }

    def transformWith[LL, RR](f: Either[L, R] => FutureEither[LL, RR])(implicit executor: ExecutionContext): FutureEither[LL, RR] = {
      safe(f(self))
    }


    def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T] = {
      safe(Future successful self.fold(l, r))
    }


    def onRight[U](f: R => U)(implicit ec: ExecutionContext): Unit = {
      self onRight f
    }

    def onLeft[U](f: L => U)(implicit ec: ExecutionContext): Unit = {
      self onLeft f
    }


    def toRight: FutureOption[R] = {
      FutureOption(self.toRight)
    }

    def toLeft: FutureOption[L] = {
      FutureOption(self.toLeft)
    }


    def await(timeout: Duration) = self


    def recover[LL >: L, RR >: R](pf: PartialFunction[Throwable, Either[LL, RR]])
      (implicit ec: ExecutionContext): FutureEither[LL, RR] = this

    def recoverWith[LL >: L, RR >: R](pf: PartialFunction[Throwable, FutureEither[LL, RR]])
      (implicit ec: ExecutionContext): FutureEither[LL, RR] = this


    override def toString = s"FutureEither($self)"


    private def safe[T](f: => Future[T]): Future[T] = {
      try f catch { case NonFatal(x) => Future failed x }
    }

    private def safe[LL, RR](f: => FutureEither[LL, RR]): FutureEither[LL, RR] = {
      try f catch { case NonFatal(x) => HasFuture(Future failed x) }
    }
  }

  implicit class MergeableFutureEither[T](private val self: FutureEither[T, T]) extends AnyVal {
    def merge(implicit ec: ExecutionContext): Future[T] = self.fold(identity, identity)
  }
}
