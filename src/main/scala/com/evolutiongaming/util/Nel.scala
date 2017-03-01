package com.evolutiongaming.util

import scala.annotation.tailrec


/**
  * Non empty list
  */
case class Nel[A](head: A, tail: List[A]) {

  def toList: List[A] = head :: tail

  def foreach[B](f: A => B): Unit = {
    f(head)
    tail foreach f
  }

  def map[B](f: A => B): Nel[B] = Nel(f(head), tail.map(f))

  def ++[AA >: A](l: List[AA]): Nel[AA] = Nel(head, tail ++ l)

  def flatMap[B](f: A => Nel[B]): Nel[B] = f(head) ++ tail.flatMap(f andThen (_.toList))

  def ::[AA >: A](a: AA): Nel[AA] = Nel(a, head :: tail)

  def filter(p: A => Boolean): List[A] = {
    val ftail = tail.filter(p)
    if (p(head)) head :: ftail
    else ftail
  }

  def filterNot(p: A => Boolean): List[A] = {
    val ftail = tail.filterNot(p)
    if (p(head)) ftail
    else head :: ftail
  }

  def concat[AA >: A](other: Nel[AA]): Nel[AA] = Nel(head, tail ::: other.toList)

  def find(p: A => Boolean): Option[A] = {
    if (p(head)) Some(head)
    else tail.find(p)
  }

  def exists(p: A => Boolean): Boolean = p(head) || tail.exists(p)

  def forall(p: A => Boolean): Boolean = p(head) && tail.forall(p)

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    tail.foldLeft(f(b, head))(f)

  def reduceLeft[AA >: A](f: (AA, AA) => AA): AA =
    tail.foldLeft[AA](head)(f)

  def reverse: Nel[A] = {
    @tailrec
    def loop(h: A, rest: List[A], acc: List[A]): Nel[A] =
      rest match {
        case Nil      => Nel(h, acc)
        case h1 :: t1 => loop(h1, t1, h :: acc)
      }
    loop(head, tail, Nil)
  }

  def mkString(start: String, sep: String, end: String): String = toList.mkString(start, sep, end)

  def mkString(sep: String): String = toList.mkString(sep)

  def mkString: String = toList.mkString

  override lazy val toString: String = s"$productPrefix($head, ${ tail mkString ", " })"
}

object Nel {

  def apply[T](head: T, tail: T*): Nel[T] = Nel(head, tail.toList)

  def opt[T](iter: Iterable[T]): Option[Nel[T]] = {
    PartialFunction.condOpt(iter.toList) { case head :: tail => Nel(head, tail) }
  }

  def unsafe[T](iter: Iterable[T]): Nel[T] = {
    val list = iter.toList
    Nel(list.head, list.tail)
  }

  def ::[T](head: T): Nel[T] = Nel(head)
}