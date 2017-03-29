package com.evolutiongaming.util

import com.typesafe.config.Config

import scala.collection.JavaConverters._
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object ConfigHelper {

  implicit class ConfigOps(val config: Config) extends AnyVal {

    def get[T](path: String)(implicit fromConf: FromConf[T]): T = fromConf(config, path)

    def getOpt[T](path: String)(implicit fromConf: FromConf[Option[T]]): Option[T] = {
      get[Option[T]](path)
    }

    def getOrElse[T: FromConf](path: String, fallbackPath: => String): T = {
      getOpt(path) getOrElse get(fallbackPath)
    }

    def getPrefixed[T: FromConf](path: String, prefix: String): T = {
      getOrElse(s"$prefix.$path", path)
    }
  }

  trait FromConf[+T] {
    def apply(config: Config, path: String): T
  }

  object FromConf {
    implicit val StringFromConf: FromConf[String] = FromConf { _ getString _ }
    implicit val BooleanFromConf: FromConf[Boolean] = FromConf { _ getBoolean _ }
    implicit val IntFromConf: FromConf[Int] = FromConf { _ getInt _ }
    implicit val LongFromConf: FromConf[Long] = FromConf { _ getLong _ }
    implicit val DoubleFromConf: FromConf[Double] = FromConf { _ getDouble _ }
    implicit val ConfigFromConf: FromConf[Config] = FromConf { _ getConfig _ }
    implicit val DurationFromConf: FromConf[FiniteDuration] = new FromConf[FiniteDuration] {
      def apply(config: Config, path: String): FiniteDuration = {
        val millis = config.getDuration(path, MILLISECONDS)
        FiniteDuration(millis, MILLISECONDS).toCoarsest.asInstanceOf[FiniteDuration]
      }
    }
    implicit val StringListFromConf: FromConf[List[String]] = FromConf {  case (conf, path) =>
      conf.getStringList(path).asScala.toList
    }
    implicit val BooleanListFromConf: FromConf[List[Boolean]] = FromConf {  case (conf, path) =>
      conf.getBooleanList(path).asScala.toList map { x => x: Boolean }
    }
    implicit val IntListFromConf: FromConf[List[Int]] = FromConf {  case (conf, path) =>
      conf.getIntList(path).asScala.toList map { x => x: Int }
    }
    implicit val LongListFromConf: FromConf[List[Long]] = FromConf {  case (conf, path) =>
      conf.getLongList(path).asScala.toList map { x => x: Long }
    }
    implicit val DoubleListFromConf: FromConf[List[Double]] = FromConf {  case (conf, path) =>
      conf.getDoubleList(path).asScala.toList map { x => x: Double }
    }
    implicit val DurationListFromConf: FromConf[List[FiniteDuration]] = FromConf {  case (conf, path) =>
      conf.getDurationList(path, MILLISECONDS).asScala.toList map { x =>
        FiniteDuration(x, MILLISECONDS).toCoarsest.asInstanceOf[FiniteDuration]
      }
    }

    implicit def optionFromConf[T](implicit fromConf: FromConf[T]): FromConf[Option[T]] = new FromConf[Option[T]] {
      def apply(config: Config, path: String): Option[T] = {
        if (config hasPath path) Some(config.get[T](path)) else None
      }
    }

    def apply[T](f: (Config, String) => T): FromConf[T] = new FromConf[T] {
      def apply(config: Config, path: String): T = f(config, path)
    }
  }
}
