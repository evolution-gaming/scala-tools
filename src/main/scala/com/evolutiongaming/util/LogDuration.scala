package com.evolutiongaming.util

import com.typesafe.scalalogging.LazyLogging

object LogDuration extends LazyLogging {

  def apply[T](name: String)(f: => T): T = {
    val start = System.currentTimeMillis()
    val result = f
    logger.debug(s"$name ${System.currentTimeMillis() - start} ms")
    result
  }
}
