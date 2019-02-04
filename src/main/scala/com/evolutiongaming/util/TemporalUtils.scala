package com.evolutiongaming.util

import java.time.temporal.{ChronoUnit, Temporal, TemporalAmount, TemporalUnit}
import java.time.{Duration, Instant}
import java.util.Date
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.{FiniteDuration, TimeUnit}

object TemporalUtils {
  implicit class RichTemporal[T <: Temporal](val timestamp: T) extends AnyVal with Ordered[T] {
    def +(duration: TemporalAmount): T = (timestamp plus duration).asInstanceOf[T]

    def +(duration: FiniteDuration): T = timestamp.plus(duration.length,
      timeUnitToTemporalUnit(duration.unit)).asInstanceOf[T]

    def -(duration: TemporalAmount): T = (timestamp minus duration).asInstanceOf[T]

    def -(duration: FiniteDuration): T = timestamp.minus(duration.length,
      timeUnitToTemporalUnit(duration.unit)).asInstanceOf[T]

    def -(other: T): Duration = Duration.between(other, timestamp)

    def compare(that: T): Int = {
      // should be at least one chrono unit
      val chronoUnit = units.find(u => u.isSupportedBy(timestamp)).head

      timestamp.until(that, chronoUnit) match {
        case 0          => 0
        case x if x < 0 => 1
        case _          => -1
      }
    }

    //For legacy compatibility only. Use java.time for new code.
    def toDate: Date = new Date(Instant.EPOCH.until(timestamp, ChronoUnit.MILLIS))
  }

  implicit class RichDuration(val d: Duration) extends AnyVal {
    def toScala: FiniteDuration = FiniteDuration(d.toNanos, TimeUnit.NANOSECONDS)
  }

  private def timeUnitToTemporalUnit(u: TimeUnit): TemporalUnit = {
    import scala.concurrent.duration._
    u match {
      case DAYS         => ChronoUnit.DAYS
      case HOURS        => ChronoUnit.HOURS
      case MINUTES      => ChronoUnit.MINUTES
      case SECONDS      => ChronoUnit.SECONDS
      case MILLISECONDS => ChronoUnit.MILLIS
      case MICROSECONDS => ChronoUnit.MICROS
      case NANOSECONDS  => ChronoUnit.NANOS
    }
  }

  private val units = ChronoUnit.values().toList

  implicit def instantOrdering: Ordering[Instant] = Ordering fromLessThan (_ isBefore _)

  implicit class NumericOps[T](val self: T) extends AnyVal {
    def toInstant(implicit numeric: Numeric[T]): Instant = Instant.ofEpochMilli(numeric.toLong(self))
  }
}
