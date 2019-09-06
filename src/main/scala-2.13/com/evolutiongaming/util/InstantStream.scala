package com.evolutiongaming.util

import java.time.Instant

/**
  * To to generate date times with some required minimal interval
  * Commonly used for data stored in MSSQL and then sorted by date time.
  */
object InstantStream {

  def apply(dateTime: Instant = Instant.now(), stepMs: Long = 10L): LazyList[Instant] = {

    def loop(dateTime: Instant): LazyList[Instant] = {
      val now = Instant.now()
      val next = if ((now.toEpochMilli - dateTime.toEpochMilli) >= stepMs) now else dateTime.plusMillis(stepMs)
      next #:: loop(next)
    }
    dateTime #:: loop(dateTime)
  }
}