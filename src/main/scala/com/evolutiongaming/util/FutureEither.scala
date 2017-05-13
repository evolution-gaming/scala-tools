package com.evolutiongaming.util

import com.evolutiongaming.util.Validation._

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Either, Failure, Success}


sealed trait FutureEither[+L, +R] {

  def future: Future[Either[L, R]]


  def map[RR](f: R => RR)(implicit ec: ExecutionContext): FutureEither[L, RR]

  def flatMap[LL >: L, RR](f: R => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR]


  def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): FutureEither[LL, R]

  def leftFlatMap[LL, RR >: R](f: L => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR]


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


  def toRight(implicit ec: ExecutionContext): FutureOption[R]

  def toLeft(implicit ec: ExecutionContext): FutureOption[L]


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
}

object FutureEither {

  def apply[L, R](x: Either[L, R]): FutureEither[L, R] = HasEither[L, R](x)

  def apply[L, R](x: R): FutureEither[L, R] = HasEither[L, R](x.ok)

  def apply[L, R](x: Future[Either[L, R]]): FutureEither[L, R] = HasFuture[L, R](x)

  def list[L, R](xs: List[FutureEither[L, R]])
    (implicit ec: ExecutionContext): FutureEither[L, List[R]] = {

    val (fs, es) = xs.foldLeft(List[HasFuture[L, R]]() -> List[HasEither[L, R]]()) {
      case ((fs, es), x) => x match {
        case x: HasFuture[L, R] => (x :: fs, es)
        case x: HasEither[L, R] => (fs, x :: es)
      }
    }

    @tailrec def loop(xs: List[Either[L, R]], result: List[R] = Nil): Either[L, List[R]] = xs match {
      case Nil            => result.reverse.ok
      case Left(x) :: _   => x.ko
      case Right(x) :: xs => loop(xs, x :: result)
    }

    if (fs.isEmpty) FutureEither(loop(es.reverse map { _.value }))
    else FutureEither(for {xs <- Future.sequence(xs map { _.future })} yield loop(xs))
  }


  private case class HasFuture[+L, +R](value: Future[Either[L, R]]) extends FutureEither[L, R] {

    def future: Future[Either[L, R]] = value


    def map[RR](f: R => RR)(implicit ec: ExecutionContext): FutureEither[L, RR] = {
      HasFuture(value map { _ map f })
    }

    def flatMap[LL >: L, RR](f: R => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR] = {
      HasFuture(for {
        x <- value
        x <- x match {
          case Left(x)  => Future successful x.ko
          case Right(x) => f(x).future
        }
      } yield x)
    }


    def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): FutureEither[LL, R] = {
      HasFuture(value map { _ leftMap f })
    }


    def leftFlatMap[LL, RR >: R](f: (L) => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR] = {
      HasFuture(for {
        x <- value
        x <- x match {
          case Left(x)  => f(x).future
          case Right(x) => Future successful x.ok
        }
      } yield x)
    }


    def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T] = {
      value map { _.fold(l, r) }
    }


    def onRight[U](f: R => U)(implicit ec: ExecutionContext): Unit = {
      value foreach { _ onRight f }
    }

    def onLeft[U](f: L => U)(implicit ec: ExecutionContext): Unit = {
      value foreach { _ onLeft f }
    }


    def toRight(implicit ec: ExecutionContext): FutureOption[R] = {
      FutureOption(value map { _.toRight })
    }

    def toLeft(implicit ec: ExecutionContext): FutureOption[L] = {
      FutureOption(value map { _.toLeft })
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


    override def toString = value.value match {
      case Some(Success(value)) => s"FutureEither($value)"
      case Some(Failure(value)) => s"FutureEither($value)"
      case None                 => "FutureEither(<not completed>)"
    }
  }


  private case class HasEither[+L, +R](value: Either[L, R]) extends FutureEither[L, R] {

    def future: Future[Either[L, R]] = Future successful value

    def map[RR](f: R => RR)(implicit ec: ExecutionContext) = {
      safe(HasEither(value map f))
    }

    def flatMap[LL >: L, RR](f: R => FutureEither[LL, RR])(implicit ec: ExecutionContext) = {
      value match {
        case Left(x)  => HasEither(x.ko)
        case Right(x) => safe(f(x))
      }
    }


    def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): FutureEither[LL, R] = {
      safe(HasEither(value leftMap f))
    }

    def leftFlatMap[LL, RR >: R](f: (L) => FutureEither[LL, RR])(implicit ec: ExecutionContext): FutureEither[LL, RR] = {
      value match {
        case Left(x)  => safe(f(x))
        case Right(x) => HasEither(x.ok)
      }
    }


    def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T] = {
      safe(Future successful value.fold(l, r))
    }


    def onRight[U](f: R => U)(implicit ec: ExecutionContext): Unit = {
      value onRight f
    }

    def onLeft[U](f: L => U)(implicit ec: ExecutionContext): Unit = {
      value onLeft f
    }


    def toRight(implicit ec: ExecutionContext): FutureOption[R] = {
      FutureOption(value.toRight)
    }

    def toLeft(implicit ec: ExecutionContext): FutureOption[L] = {
      FutureOption(value.toLeft)
    }


    def await(timeout: Duration) = value


    def recover[LL >: L, RR >: R](pf: PartialFunction[Throwable, Either[LL, RR]])
      (implicit ec: ExecutionContext): FutureEither[LL, RR] = this

    def recoverWith[LL >: L, RR >: R](pf: PartialFunction[Throwable, FutureEither[LL, RR]])
      (implicit ec: ExecutionContext): FutureEither[LL, RR] = this

    
    override def toString = s"FutureEither($value)"


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