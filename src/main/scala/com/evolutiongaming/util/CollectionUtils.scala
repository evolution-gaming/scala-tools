package com.evolutiongaming.util

object CollectionUtils {

  implicit class MapOpsStrict[K, V](val self: Map[K, V]) extends AnyVal {
    def filterKeysStrict(p: K => Boolean): Map[K, V] = self.collect {
      case (k, v) if p(k) => (k, v)
    }
    def mapValuesStrict[V1](f: V => V1): Map[K, V1] = self.map {
      case (k, v) => (k, f(v))
    }
  }
}
