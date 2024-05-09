package com.evolutiongaming.util

object RichClass {

  implicit class RichClassOps(val self: Class[_]) extends AnyVal {
    def simpleName: String = {
      val xs = self.getName.split("\\$")
      if (xs.size > 1) xs.last else self.getSimpleName
    }
  }
}
