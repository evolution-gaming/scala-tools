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
  *
  * Purpose of `let` and `|>` methods to evolve an operation within a method chain.
  *
  * What used to be
  *
  *   f1(f2(f3(a)))
  *
  * becomes
  *
  *   a |> f3 |> f2 |> f1
  *
  * What used to be
  *
  *   system.settings.config.getConfig("company.http.client")
  *     .withFallback(system.settings.config.getConfig("akka.http.client"))
  *
  * becomes
  *
  *   (system.settings.config.getConfig _).let { config =>
  *     config("company.http.client") withFallback config("akka.http.client")
  *   }
  *
  */

object Tap {
  implicit class Ops[A](val a: A) extends AnyVal {
    def tap[B](f: A => B): A = { f(a); a }
    def let[B](f: A => B): B = f(a)
    def |>[B](f: A => B): B = f(a)
  }
}
