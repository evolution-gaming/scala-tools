package com.evolutiongaming.util

import scala.concurrent.ExecutionContextExecutor

/**
  * For tests only
  */
object CurrentThreadExecutionContext extends ExecutionContextExecutor {
  def execute(runnable: Runnable) = runnable.run()
  def reportFailure(cause: Throwable) = throw cause
}
