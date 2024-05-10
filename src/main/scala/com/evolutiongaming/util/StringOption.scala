package com.evolutiongaming.util

object StringOption extends (String => Option[String]) {
  def apply(s: String): Option[String] = Option(s) map (_.trim) filter (_.nonEmpty)
}
