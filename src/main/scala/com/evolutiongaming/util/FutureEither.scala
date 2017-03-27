package com.evolutiongaming.util

import com.evolutiongaming.util.Validation._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.Either
import scala.util.control.NonFatal


sealed trait FutureEither[+L, +R] {

  def future: Future[R]

  def either(implicit ec: ExecutionContext): Future[Either[L, R]]

  def map[RR](f: R => RR)(implicit ec: ExecutionContext): FutureEither[L, RR]

  def flatMap[LL >: L, RR](f: (R) => FutureEither[LL, RR])(implicit ec: ExecutionContext, tag: ClassTag[LL]): FutureEither[LL, RR]

  def result(timeout: Duration): Either[L, R]

  def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext, tag: ClassTag[LL]): FutureEither[LL, R]

  def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T]

  def getOrElse[RR >: R](l: L => RR)(implicit ec: ExecutionContext): Future[RR] = fold[RR](l, identity)

  def recover[LL >: L, RR >: R](pf: PartialFunction[Throwable, Either[LL, RR]])
    (implicit ec: ExecutionContext, tag: ClassTag[LL]): FutureEither[LL, RR]

  def onFailure[LL >: L](f: L => Any)(implicit ec: ExecutionContext): Unit

  def recoverWith[LL >: L, RR >: R](pf: PartialFunction[L, Either[LL, RR]])
    (implicit ec: ExecutionContext, tag: ClassTag[LL]): FutureEither[LL, RR] = {

    val future = for {either <- either} yield either.recoverWith(pf)
    FutureEither(future)
  }
}

object FutureEither {

  def apply[L: ClassTag, R](x: Either[L, R]): FutureEither[L, R] = HasEither[L, R](x)

  def apply[L: ClassTag, R](x: R): FutureEither[L, R] = HasEither[L, R](x.ok)

  def apply[L, R](x: Future[Either[L, R]])(implicit ex: ExecutionContext, tag: ClassTag[L]): FutureEither[L, R] = {
    HasFuture(for {x <- x} yield x getOrElse { x => throw LeftFailure(x) })
  }

  def right[L, R](x: Future[R])(implicit tag: ClassTag[L]): FutureEither[L, R] = HasFuture(x)

  def sequence[L, R](xs: List[FutureEither[L, R]])
    (implicit ec: ExecutionContext, tag: ClassTag[L]): FutureEither[L, List[R]] = {

    val (fs, es) = xs.foldLeft(List[HasFuture[L, R]]() -> List[HasEither[L, R]]()) {
      case ((fs, es), x) => x match {
        case x: HasFuture[L, R] => (x :: fs, es)
        case x: HasEither[L, R] => (fs, x :: es)
      }
    }

    def sequence(xs: List[HasEither[L, R]], result: List[R]): Either[L, List[R]] = xs match {
      case Nil     => result.ok
      case x :: xs => x.value.flatMap(x => sequence(xs, x :: result))
    }

    val e = FutureEither.HasEither(sequence(es, Nil))
    if (fs.isEmpty) e
    else for {
      k <- e
      d <- FutureEither.HasFuture[L, List[R]](Future.sequence(fs.map(_.future)))
    } yield k ::: d
  }


  case class LeftFailure(left: Any) extends Exception(s"FutureEither.Left($left)", null, false, false)

  private case class HasFuture[+L, R](future: Future[R])(implicit tag: ClassTag[L]) extends FutureEither[L, R] {

    def either(implicit ec: ExecutionContext) = fold(_.ko, _.ok)

    def map[RR](f: R => RR)(implicit ec: ExecutionContext): FutureEither[L, RR] = {
      HasFuture[L, RR](future map f)
    }

    def flatMap[LL >: L, RR](f: (R) => FutureEither[LL, RR])(implicit ec: ExecutionContext, tag: ClassTag[LL]): FutureEither[LL, RR] = {
      HasFuture[LL, RR](future.flatMap(x => f(x).future))
    }

    def result(timeout: Duration): Either[L, R] = try Await.result(future, timeout).ok catch {
      case LeftFailure(tag(failure)) => failure.ko
      case NonFatal(failure)         => throw failure
    }

    def leftMap[LL](f: (L) => LL)(implicit ec: ExecutionContext, tag: ClassTag[LL]) = {
      HasFuture[LL, R](future recover { case LeftFailure(this.tag(l)) => throw LeftFailure(f(l)) })
    }

    def fold[T](l: (L) => T, r: (R) => T)(implicit ec: ExecutionContext) = {
      future map r recover { case LeftFailure(tag(failure)) => l(failure) }
    }

    def recover[LL >: L, RR >: R](pf: PartialFunction[Throwable, Either[LL, RR]])
      (implicit ec: ExecutionContext, tag: ClassTag[LL]): FutureEither[LL, RR] = {

      HasFuture[LL, RR](future recover {
        case x: LeftFailure         => throw x
        case x if pf.isDefinedAt(x) => pf(x) getOrElse { l => throw LeftFailure(l) }
      })
    }

    def onFailure[LL >: L](f: L => Any)(implicit ec: ExecutionContext) = future onFailure {
      case LeftFailure(this.tag(l)) => f(l)
    }

    override def toString = s"$productPrefix($future)"
  }

  private case class HasEither[+L, R](value: Either[L, R]) extends FutureEither[L, R] {

    lazy val future = value.fold(x => Future.failed(LeftFailure(x)), Future.successful)

    def either(implicit ec: ExecutionContext) = Future successful value

    def flatMap[LL >: L, RR](f: R => FutureEither[LL, RR])(implicit ec: ExecutionContext, tag: ClassTag[LL]) = {
      def safeF(r: R): FutureEither[LL, RR] = try f(r) catch {
        case NonFatal(e) => HasFuture[LL, RR](Future failed e)
      }
      value.fold(l => HasEither(l.ko), safeF)
    }

    def result(timeout: Duration) = value

    def map[RR](f: R => RR)(implicit ec: ExecutionContext) = {
      try HasEither(value map f) catch {
        case NonFatal(e) => HasFuture[Nothing, RR](Future failed e)
      }
    }

    def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext, tag: ClassTag[LL]) = HasEither(value leftMap f)

    def fold[T](l: (L) => T, r: (R) => T)(implicit ec: ExecutionContext) = Future successful value.fold(l, r)

    def recover[LL >: L, RR >: R](pf: PartialFunction[Throwable, Either[LL, RR]])
      (implicit ec: ExecutionContext, tag: ClassTag[LL]): FutureEither[LL, RR] = this


    def onFailure[LL >: L](f: L => Any)(implicit ec: ExecutionContext) = value.onFailure(f)

    override def toString = s"$productPrefix($value)"
  }

  implicit class MergeableFutureEither[T](private val self: FutureEither[T, T]) extends AnyVal {
    def merge(implicit ec: ExecutionContext): Future[T] = self.fold(identity, identity)
  }
}