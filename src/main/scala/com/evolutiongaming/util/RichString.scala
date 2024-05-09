package com.evolutiongaming.util

object RichString {

  implicit class RichStringOps(val self: String) extends AnyVal {

    def toIntOpt: Option[Int] = numeric(_.toInt)

    private def numeric[T](f: String => T): Option[T] = try StringOption(self).map(f) catch {
      case _: NumberFormatException => None
    }
  }

}
