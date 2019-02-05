package com.evolutiongaming.util

/**
  * Purpose of `tap` method is to perform operations on intermediate results within a method chain.
  *
  * (1 to 10)
  *   .tap { x => println(s"original: $x") }
  *   .filter { _ % 2 == 0 }
  *   .tap { x => println(s"evens: $x") }
  *   .map { x => x * x }
  *   .tap { x => println(s"squares: $x") }
  *
  * What used to be
  *
  *   def stackTraceAsString(cause: Throwable): String = {
  *     val out = new StringWriter()
  *     cause.printStackTrace(new PrintWriter(out))
  *     out.toString
  *   }
  *
  * becomes
  *
  *   def stackTraceAsString(cause: Throwable): String =
  *     new StringWriter() tap { out => cause.printStackTrace(new PrintWriter(out)) } toString
  *
  */

object Tap {
  implicit class Ops[A](val a: A) extends AnyVal {
    def tap(f: A => Unit): A = { f(a); a }
  }
}
