package com.evolutiongaming.util

import com.typesafe.config.Config

import scala.concurrent.duration._

object ConfigHelpers {

  implicit class RichConfig(val config: Config) extends AnyVal {

    def duration(path: String): FiniteDuration = {
      val millis = config.getDuration(path, MILLISECONDS)
      FiniteDuration(millis, MILLISECONDS).toCoarsest.asInstanceOf[FiniteDuration]
    }

    def configOpt(path: String): Option[Config] =
      if(config hasPath path) Some(config getConfig path) else None

    def getConfig(prefix: String, path: String): Config = {
      val combined = s"$prefix.$path"
      if (config hasPath combined) config getConfig combined else config getConfig path
    }

    def durationOpt(path: String): Option[FiniteDuration] =
      if(config hasPath path) Some(duration(path)) else None

    def intOpt(path: String): Option[Int] =
      if(config hasPath path) Some(config getInt path) else None

    def booleanOpt(path: String): Option[Boolean] =
      if(config hasPath path) Some(config getBoolean path) else None

    def stringOpt(path: String): Option[String] =
      if(config hasPath path) Some(config getString path) else None

    def prefixConfig(path: String, prefix: String) =
      if (prefix.nonEmpty && (config hasPath s"$prefix.$path")) config getConfig prefix else config

    def intPrefixed(path: String, prefix: String): Int = prefixConfig(path, prefix) getInt path

    def stringPrefixed(path: String, prefix: String): String = prefixConfig(path, prefix) getString path

    def durationPrefixed(path: String, prefix: String): FiniteDuration = prefixConfig(path, prefix) duration path
  }
}
