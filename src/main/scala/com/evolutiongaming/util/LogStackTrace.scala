package com.evolutiongaming.util

import com.typesafe.scalalogging.LazyLogging

object LogStackTrace extends LazyLogging {
  def apply(name: String = "", depth: Int = 20): Unit = func(name, depth)()

  def func(name: String = "", depth: Int = 20): (() => Unit) = {
    if (logger.underlying.isDebugEnabled) {
      val thread = Thread.currentThread()
      val stackTrace = thread.getStackTrace
        .drop(1)
        .dropWhile(_.getClassName == getClass.getName)
        .take(depth)
      val prefix = if (name.nonEmpty) name else stackTrace.head.getMethodName
      val s = stackTrace.mkString(s"$prefix\n\t", "\n\t", "\n")
      () => logger.debug(s)
    } else {
      () => ()
    }
  }
}