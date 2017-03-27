package com.evolutiongaming.util

import com.typesafe.scalalogging.LazyLogging

import scala.compat.Platform

object LogDuration extends LazyLogging {
  def apply[T](name: String)(f: => T): T = {
    val start = Platform.currentTime
    val result = f
    logger.debug(s"$name ${Platform.currentTime - start} ms")
    result
  }
}
