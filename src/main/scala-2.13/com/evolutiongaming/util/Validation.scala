package com.evolutiongaming.util

import com.evolutiongaming.concurrent.CurrentThreadExecutionContext
import com.github.t3hnar.scalax._

import scala.annotation.tailrec
import scala.collection.BuildFrom
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.util.{Either, Failure, Left, Right, Success, Try}


object Validation {

  type V[+T] = Either[String, T]

  type FutureV[T] = FutureEither[String, T]

  type FE[L, R] = FutureEither[L, R]

  type FV[R] = FutureEither[String, R]

  type Error = Left[String, Nothing]

  type FO[T] = FutureOption[T]


  implicit class EitherOpsValidation[+L, +R](val self: Either[L, R]) extends AnyVal {

    def leftMap[LL](f: L => LL): Either[LL, R] = self match {
      case Left(x)  => Left(f(x))
      case Right(x) => Right(x)
    }

    def leftFlatMap[LL, RR >: R](f: L => Either[LL, RR]): Either[LL, RR] = self match {
      case Left(x)  => f(x)
      case Right(x) => Right(x)
    }


    def foldRight[RR >: R](f: L => RR): RR = self.fold(f, identity)

    def foldLeft[LL >: L](f: R => LL): LL = self.fold(identity, f)


    def orElse[LL, RR >: R](or: => Either[LL, RR]): Either[LL, RR] = leftFlatMap(_ => or)

    def |[RR >: R](or: => RR): RR = self.getOrElse(or)

    def orError(toError: L => Throwable = x => new LeftException(x.toString)): R = {
      this foldRight { l => throw toError(l) }
    }


    def onRight[U](f: R => U): Unit = self match {
      case Left(_)  =>
      case Right(x) => f(x); ()
    }

    def onLeft[U](f: L => U): Unit = self match {
      case Left(x)  => f(x); ()
      case Right(_) =>
    }


    def toRight: Option[R] = self match {
      case Left(_)  => None
      case Right(x) => Some(x)
    }

    def toLeft: Option[L] = self match {
      case Left(x)  => Some(x)
      case Right(_) => None
    }

    def toList: List[R] = self match {
      case Left(_)  => Nil
      case Right(x) => x :: Nil
    }

    def toIterable: Iterable[R] = toList


    def isOk: Boolean = self.isRight

    def isKo: Boolean = self.isLeft

    def exists(f: R => Boolean): Boolean = self match {
      case Right(x) => f(x)
      case Left(_)  => false
    }

    def contains[RR >: R](x: RR): Boolean = self match {
      case Right(`x`) => true
      case _          => false
    }

    def forall(f: R => Boolean): Boolean = self match {
      case Right(b) => f(b)
      case Left(_)  => true
    }


    def ?>>[LL](x: => LL): Either[LL, R] = leftMap { _ => x }

    def fe[AA >: L]: FutureEither[AA, R] = FutureEither[AA, R](self)


    def collect[RR >: R](pf: PartialFunction[R, RR]): Either[L, RR] = {
      self.map { x => if (pf isDefinedAt x) pf(x) else x }
    }

    def collectWith[LL >: L, RR >: R](pf: PartialFunction[R, Either[LL, RR]]): Either[LL, RR] = {
      self.flatMap { x => if (pf isDefinedAt x) pf(x) else Right(x) }
    }


    def recover[RR >: R](pf: PartialFunction[L, RR]): Either[L, RR] = {
      leftFlatMap { x => if (pf isDefinedAt x) Right(pf(x)) else Left(x) }
    }

    def recoverWith[AA >: L, BB >: R](pf: PartialFunction[L, Either[AA, BB]]): Either[AA, BB] = {
      leftFlatMap { x => if (pf isDefinedAt x) pf(x) else Left(x) }
    }


    def fallbackTo[LL >: L, BB >: R](that: => Either[LL, BB]): Either[LL, BB] = {
      leftFlatMap { l => that leftMap { _ => l } }
    }
  }

  implicit class BoolToEitherOpsValidation(val self: Boolean) extends AnyVal {
    def trueOr[T](left: => T): Either[T, Unit] = {
      if (self) ().ok else left.ko
    }

    def falseOr[T](left: => T): Either[T, Unit] = {
      if (self) left.ko else ().ok
    }
  }

  /**
    * Useful implicits when dealing with lots of legacy nullable Java code
    * The codes becomes less Lisp-y and more Groovy/Ruby style,
    * that I consider rather good thing than bad
    */
  implicit class NullableToValidatedOpsValidation[A](val self: A) extends AnyVal {

    /** Creates Some(x) if the caller is not null,
      * and None if it is null.
      *
      * @return Some(value) if value != null, None if value == null
      */
    def ? : Option[A] = Option(self)

    /** Returns a [[scala.util.Left]] containing the given
      * argument `left` if this `self` is empty, or
      * a [[scala.util.Right]] containing this `o`'s value if
      * this is nonempty.
      *
      * @param left the expression to evaluate and return if this is empty
      * @see toLeft
      */
    def ?>>[B](left: => B): Either[B, A] = this.?.toRight(left)

    def fo: FutureOption[A] = FutureOption(Option(self))

    def asInstanceV[B <: A](implicit tag: ClassTag[B]): V[B] = {
      self.asInstanceOfOpt[B] ?>> s"type mismatch, expected: ${ tag.runtimeClass.simpleName }, actual: ${ self.getClass.simpleName }"
    }
  }

  implicit class OptionOpsValidation[+T](val self: Option[T]) extends AnyVal {

    /** Returns a [[scala.util.Left]] containing the given
      * argument `left` if this `o` is empty, or
      * a [[scala.util.Right]] containing this `o`'s value if
      * this is nonempty.
      *
      * @param left the expression to evaluate and return if this is empty
      * @see toLeft
      */
    def ?>>[L](left: => L): Either[L, T] = self.toRight(left)

    /** Returns the option's value if the option is nonempty, otherwise
      * return the result of evaluating `default`.
      *
      * @param default the default expression.
      */
    def |[TT >: T](default: => TT): TT = self.getOrElse(default)

    def fo: FutureOption[T] = FutureOption(self)
  }

  implicit class TryOpsValidation[+A](val t: Try[A]) extends AnyVal {
    /** Returns a [[scala.util.Left]] containing the given
      * argument `left` if this `t` has failed, or
      * a [[scala.util.Right]] containing this `t` has been successful.
      *
      * @param left the expression to evaluate and return if this is a failure
      * @see toLeft
      */
    def ?>>[B](left: => B): Either[B, A] = t.toOption.toRight(left)

    def leftMapToEither[B](f: Throwable => B): Either[B, A] = t match {
      case Success(right) => Right(right)
      case Failure(e)     => Left(f(e))
    }
  }

  implicit class ToEitherOpsValidation[A](val self: A) extends AnyVal {
    def right[B]: Either[B, A] = Right(self)
    def left[B]: Either[A, B] = Left(self)
    def success[B]: Either[B, A] = Right(self)
    def fail[B]: Either[A, B] = Left(self)
    def ok[B]: Either[B, A] = Right(self)
    def ko[B]: Either[A, B] = Left(self)
  }

  implicit class IterableVOpsValidation[L, R](val self: Iterable[Either[L, R]]) extends AnyVal {

    def takeValid: Either[L, Iterable[R]] = {
      if (self.isEmpty) Iterable.empty.ok
      else {
        val valid = self.collect { case Right(x) => x }
        if (valid.nonEmpty) valid.ok
        else self.collectFirst { case Left(x) => x }.get.ko
      }
    }

    def separate: (Iterable[L], Iterable[R]) = {
      val ls = self collect { case Left(x) => x }
      val rs = self collect { case Right(x) => x }
      (ls, rs)
    }
  }

  implicit class IterableOpsValidation[A, I[X] <: IterableOnce[X]](val self: I[A]) extends AnyVal {

    def ?>>[B](left: => B): Either[B, I[A]] = if (self.iterator.isEmpty) Left(left) else Right(self)

    /**
      * Checks that all elements in `self` are valid by applying the `test` function to each.
      * If all are valid, returns a [[scala.util.Right]] with a collection of whatever "good"
      * values `test` returns. Otherwise returns the first [[scala.util.Left]] encountered.
      *
      * @param test function to apply to each element
      * @tparam L type of error the `test` function returns
      * @tparam R type of the good value the `test` function produces
      */
    def allValid[L, R, To](test: A => Either[L, R])(implicit buildFrom: BuildFrom[I[A], R, To]): Either[L, To] = {
      val b = buildFrom.newBuilder(self)

      @tailrec
      def loop(it: List[A]): Either[L, To] = {
        it match {
          case a :: as => test(a) match {
            case Left(l)  => Left(l)
            case Right(r) => b += r; loop(as)
          }
          case Nil    => b.result().ok
        }
      }
      loop(self.iterator.to(List))
    }
  }

  implicit class FutureOfEitherOpsValidation[L, R](val self: Future[Either[L, R]]) extends AnyVal {
    def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): Future[Either[LL, R]] = {
      self.map(e => e.leftMap(f))
    }

    def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T] = {
      self map ((e: Either[L, R]) => e.fold(l, r))
    }

    def fe: FutureEither[L, R] = FutureEither(self)

    def orElse[LL >: L, RR >: R](right: => Future[Either[LL, RR]])(implicit ec: ExecutionContext): Future[Either[LL, RR]] = {
      val p = Promise[Either[LL, RR]]()
      self.onComplete {
        case Success(r @ Right(_)) => p.success(r)
        case _                     => p.completeWith(right)
      }
      p.future
    }
  }

  implicit class FutureOfOptionOpsValidation[T](val self: Future[Option[T]]) extends AnyVal {

    def fold[TT](l: => TT, r: T => TT)(implicit ec: ExecutionContext): Future[TT] = {
      self map (e => e.fold(l)(r))
    }

    def fe: FutureEither[Unit, T] = self.map(_.toRight(()))(CurrentThreadExecutionContext).fe

    def fo: FutureOption[T] = FutureOption(self)

    def orElse[TT >: T](right: => Future[Option[TT]])(implicit ec: ExecutionContext): Future[Option[TT]] = {
      val p = Promise[Option[TT]]()
      self.onComplete {
        case Success(r @ Some(_)) => p.success(r)
        case _                    => p.completeWith(right)
      }
      p.future
    }
  }

  implicit class NestedEitherOpsValidation[+A, +B](val e: Either[A, Either[A, B]]) extends AnyVal {
    def flatten: Either[A, B] = e.flatMap(identity)
  }

  implicit class FutureOfAnyOpsValidation[T](val self: Future[T]) extends AnyVal {
    def fo: FutureOption[T] = FutureOption(self.map(Option.apply)(CurrentThreadExecutionContext))

    def fe[L]: FutureEither[L, T] = FutureEither(self.map(Right.apply)(CurrentThreadExecutionContext))
  }
}
