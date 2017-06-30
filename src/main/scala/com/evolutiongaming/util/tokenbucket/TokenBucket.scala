/*
 * Copyright 2012-2014 Brandon Beck (https://github.com/bbeck/token-bucket)
 * Copyright 2016 Sergiy Prydatchenko (Evolution Gaming)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.evolutiongaming.util.tokenbucket

import java.util.concurrent.atomic.AtomicLong

case class TokenBucket(
  ratePerPeriod: Long,
  periodInMillis: Long = 1000L) {

  private val size = new AtomicLong(ratePerPeriod)

  private val refillStrategy = FixedIntervalRefillStrategy(ratePerPeriod, periodInMillis)

  def currentSize: Long = {
    refill(refillStrategy.refill())
    size.get()
  }

  def tryConsume(numTokens: Long = 1L): Boolean = {
    if (numTokens < 1L) throw new IllegalArgumentException("Number of tokens to consume must be > 1")

    refill(refillStrategy.refill())

    if (numTokens > size.get())
      false
    else {
      // as it's not synchronized here, it may cause very small amount of non-equal distribution between periods
      size getAndAdd -numTokens
      true
    }
  }

  def refill(numTokens: Long): Unit = {
    if (numTokens != 0L) {
      // as it's not synchronized here, it may cause very small amount of false positives
      if ((size addAndGet numTokens) > ratePerPeriod) size set ratePerPeriod
    }
  }
}
