package com.evolutiongaming.util

import java.util.concurrent.Executor

import scala.concurrent.ExecutionContext

/**
 * For tests only
 */
object CurrentThreadExecutionContext extends ExecutionContext with Executor {
  def execute(runnable: Runnable) = runnable.run()
  def reportFailure(cause: Throwable) = throw cause
}
