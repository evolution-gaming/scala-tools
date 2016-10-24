package com.evolutiongaming.util

object Run {
  def apply(f: => Unit): Runnable = new Runnable {def run() = { f }}
}