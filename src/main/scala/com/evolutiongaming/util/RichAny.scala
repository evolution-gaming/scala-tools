package com.evolutiongaming.util

import scala.reflect.ClassTag

object RichAny {

  implicit class RichAnyOps[A](val self: A) extends AnyVal {
    def asInstanceOfOpt[T <: A](implicit tag: ClassTag[T]): Option[T] = asInstanceOfOptUnsafe[T]

    /**
     * "".asInstanceOfOptUnsafe[Int] compiles
     * "".asInstanceOfOpt[Int] does not compile
     */
    def asInstanceOfOptUnsafe[T](implicit tag: ClassTag[T]): Option[T] = tag.unapply(self)
  }
}
