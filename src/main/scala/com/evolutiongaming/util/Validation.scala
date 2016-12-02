package com.evolutiongaming.util

import com.github.t3hnar.scalax._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{Either, Failure, Success, Try}

/**
 * @author Sergey Tavanets
 */
object Validation {

  type V[+T] = Either[String, T]

  type FutureV[T] = FutureEither[String, T]

  type Error = Left[String, Nothing]

  object FutureV {
    def apply[T](future: Future[Either[String, T]])(implicit ec: ExecutionContext): FutureV[T] = {
      FutureEither(future recover pf)
    }

    def right[T](future: Future[T])(implicit ec: ExecutionContext): FutureV[T] = {
      FutureEither right future recover pf
    }

    private def pf[T]: PartialFunction[Throwable, Either[String, T]] = {
      case NonFatal(e) => s"Exception: ${e.getMessage}".ko
    }
  }

  implicit class RightBiasedEitherOps[+A, +B](val e: Either[A, B]) extends AnyVal {
    def map[C](f: B => C): Either[A, C] = e.right.map(f)

    def foreach[U](f: B => U): Unit = e.right.foreach(f)

    def flatMap[AA >: A, Y](f: B => Either[AA, Y]): Either[AA, Y] = e.right.flatMap(f)

    def toOption: Option[B] = e.right.toOption

    def toList: List[B] = e.right.toSeq.toList

    def toIterable: Iterable[B] = e.right.toSeq

    def orElse[AA >: A, BB >: B](or: A => Either[AA, BB]): Either[AA, BB] = e.left.flatMap(or)

    def getOrElse[BB >: B](or: A => BB): BB = e.fold[BB](or, identity)

    def orFailure[BB >: B](or: => BB): BB = e.right.getOrElse(or)

    def onFailure[U](f: A => U): Unit = e.left.foreach(f)

    def toFailure: Option[A] = e.left.toOption

    def |[BB >: B](or: => BB): BB = e.right.getOrElse(or)

    def leftMap[AA](f: A => AA): Either[AA, B] = e.left.map(f)

    def orError: B = this getOrElse { error => sys error error.toString }

    def isOk: Boolean = e.isRight

    def isKo: Boolean = e.isLeft

    def exists(f: B => Boolean) = e.right exists f

    def contains[BB >: B](x: BB): Boolean = exists(_ == x)

    def collect[AA >: A, BB](error: B => AA, pf: PartialFunction[B, BB]): Either[AA, BB] = {
      e.right.flatMap { x => if (pf isDefinedAt x) Right(pf(x)) else Left(error(x)) }
    }

    def ?>>[AA](x: => AA): Either[AA, B] = leftMap { _ => x }

    def fe[AA >: A](implicit tag: ClassTag[AA]): FutureEither[AA, B] = FutureEither[AA, B](e)
  }

  implicit class BoolToValidatedOps(val self: Boolean) extends AnyVal {
    def trueOr[T](left: => T): Either[T, Unit] = {
      if (self) Right(()) else Left(left)
    }

    def falseOr[T](left: => T): Either[T, Unit] = {
      if (self) Left(left) else Right(())
    }
  }

  /**
   * Useful implicits when dealing with lots of legacy nullable Java code
   * The codes becomes less Lisp-y and more Groovy/Ruby style,
   * that I consider rather good thing than bad
   */
  implicit class NullableToValidatedOps[A](val self: A) extends AnyVal {

    /** Creates Some(x) if the caller is not null,
      *  and None if it is null.
      *
      *  @return   Some(value) if value != null, None if value == null
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


    def asInstanceV[B <: A](implicit tag: ClassTag[B]): V[B] = {
      self.asInstanceOfOpt[B] ?>> s"type mismatch, expected: ${tag.runtimeClass.simpleName}, actual: ${self.getClass.simpleName}"
    }
  }

  implicit class OptionOps[+A](val o: Option[A]) extends AnyVal {

    /** Returns a [[scala.util.Left]] containing the given
      * argument `left` if this `o` is empty, or
      * a [[scala.util.Right]] containing this `o`'s value if
      * this is nonempty.
      *
      * @param left the expression to evaluate and return if this is empty
      * @see toLeft
      */
    def ?>>[B](left: => B): Either[B, A] = o.toRight(left)

    /** Returns the option's value if the option is nonempty, otherwise
      * return the result of evaluating `default`.
      *
      *  @param default  the default expression.
      */
    def |[B >: A](default: => B): B = o.getOrElse(default)
  }

  implicit class TryOps[+A](val t: Try[A]) extends AnyVal {
    /** Returns a [[scala.util.Left]] containing the given
      * argument `left` if this `t` has failed, or
      * a [[scala.util.Right]] containing this `t` has been successful.
      *
      * @param left the expression to evaluate and return if this is a failure
      * @see toLeft
      */
    def ?>>[B](left: ⇒ B): Either[B, A] = t.toOption.toRight(left)

    def leftMapToEither[B](f: Throwable => B): Either[B, A] = t match {
      case Success(right) => Right(right)
      case Failure(e)     => Left(f(e))
    }
  }

  implicit class ToEitherOps[A](val self: A) extends AnyVal {
    def right[B]: Either[B, A] = Right(self)
    def left[B]: Either[A, B] = Left(self)
    def success[B]: Either[B, A] = Right(self)
    def fail[B]: Either[A, B] = Left(self)
    def ok[B]: Either[B, A] = Right(self)
    def ko[B]: Either[A, B] = Left(self)
  }

  implicit class IterableVOps[L, R](val self: Iterable[Either[L, R]]) extends AnyVal {

    def takeValid: Either[L, Iterable[R]] = {
      if (self.isEmpty) Iterable.empty.ok
      else {
        val valid = self.collect { case Right(x) => x }
        if (valid.nonEmpty) valid.ok
        else self.collectFirst { case Left(x) => x }.get.ko
      }
    }

    def separate: (Iterable[L], Iterable[R]) = {
      val valid = self.collect { case Right(x) => x }
      val invalid = self.collect { case Left(x) => x }
      (invalid, valid)
    }
  }

  implicit class IterableOps[A](val self: Iterable[A]) extends AnyVal {

    def ?>>[B](left: => B): Either[B, Iterable[A]] = if (self.isEmpty) Left(left) else Right(self)

    /**
      * Checks that all elements in `self` are valid by applying the `test` function to each.
      * If so, returns a [[scala.util.Right]] otherwise returns the first [[scala.util.Left]]
      * encountered.
      *
      * @param test function to apply to each element
      * @tparam T type of error the `test` function returns
      */
    def allValid[T](test: A => Either[T, Unit]): Either[T, Unit] = {
      @tailrec
      def loop(it: Iterable[A]): Either[T, Unit] = {
        it.headOption match {
          case Some(a) => test(a) match {
            case l@Left(_) => l
            case Right(_)  => loop(it.tail)
          }
          case None    => ().ok
        }
      }
      loop(self)
    }
  }

  implicit class VOps[T](val self: V[T]) extends AnyVal {
    def collect[TT >: T](pf: PartialFunction[T, TT]): V[TT] = {
      RightBiasedEitherOps(self).collect(x => s"PartialFunction is not defined at $x", pf)
    }
  }

  implicit class FutureOfEitherOps[L, R](val self: Future[Either[L, R]]) extends AnyVal {
    def leftMap[LL](f: L => LL)(implicit ec: ExecutionContext): Future[Either[LL, R]] = {
      self.map(e => e.leftMap(f))
    }

    def fold[T](l: L => T, r: R => T)(implicit ec: ExecutionContext): Future[T] = {
      self map ((e: Either[L, R]) => e.fold(l, r))
    }

    def fe(implicit ec: ExecutionContext, tag: ClassTag[L]): FutureEither[L, R] = FutureEither(self)

    def orElse[LL >: L, RR >: R](right: => Future[Either[LL, RR]])(implicit ec: ExecutionContext): Future[Either[LL, RR]] = {
      val p = Promise[Either[LL, RR]]()
      self.onComplete {
        case Success(r@Right(_)) => p.success(r)
        case _ => p.completeWith(right)
      }
      p.future
    }
  }

  implicit class FutureOfOptionOps[R](val self: Future[Option[R]]) extends AnyVal {
    def fold[T](l: => T, r: R => T)(implicit ec: ExecutionContext): Future[T] = {
      self map (e => e.fold(l)(r))
    }

    def fe(implicit ec: ExecutionContext): FutureEither[Unit, R] = self.map(_.toRight(())).fe

    def orElse[RR >: R](right: => Future[Option[RR]])(implicit ec: ExecutionContext): Future[Option[RR]] = {
      val p = Promise[Option[RR]]()
      self.onComplete {
        case Success(r@Some(_)) => p.success(r)
        case _ => p.completeWith(right)
      }
      p.future
    }
  }

  implicit class NestedEitherOps[+A, +B](val e: Either[A, Either[A, B]]) extends AnyVal {
    def flatten: Either[A, B] = e.flatMap(identity)
  }
}
