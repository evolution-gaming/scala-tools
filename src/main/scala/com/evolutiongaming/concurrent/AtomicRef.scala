package com.evolutiongaming.concurrent

import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

trait AtomicRef[T] {
  def apply(): T
  def updateAndGet(f: T => T): T
  def getAndUpdate(f: T => T): T
  def set(x: T): Unit
  def getAndSet(x: T): T
  def compareAndSet(expected: T, update: T): Boolean
}

object AtomicRef {
  def apply[T](value: T): AtomicRef[T] = {
    new Impl(new AtomicReference[T](value))
  }

  private def unary[T](f: T => T) = new UnaryOperator[T] {def apply(x: T): T = f(x) }

  private class Impl[T](ref: AtomicReference[T]) extends AtomicRef[T] {

    def apply(): T = ref.get()

    def updateAndGet(f: (T) => T): T = ref.updateAndGet(unary(f))

    def getAndUpdate(f: (T) => T): T = ref.getAndUpdate(unary(f))

    def set(x: T): Unit = ref.set(x)

    def getAndSet(x: T): T = ref.getAndSet(x)

    def compareAndSet(expected: T, update: T): Boolean = ref.compareAndSet(expected, update)

    override def toString = s"AtomicRef(${ apply() })"
  }
}