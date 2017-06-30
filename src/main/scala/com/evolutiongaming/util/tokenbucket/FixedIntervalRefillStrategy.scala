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

case class FixedIntervalRefillStrategy(
  numTokensPerPeriod: Long,
  periodInMillis: Long) {

  private val periodDurationInNanos = periodInMillis * 1000000

  private def nanoTime: Long = System.nanoTime()

  @volatile private var lastRefillTime: Long = nanoTime
  @volatile private var nextRefillTime: Long = nanoTime

  def refill(): Long = {
    val now = nanoTime
    if (now < nextRefillTime) {
      0
    } else {
      this synchronized {
        val numPeriods = Math.max(0, (now - lastRefillTime) / periodDurationInNanos)
        lastRefillTime += numPeriods * periodDurationInNanos
        nextRefillTime = lastRefillTime + periodDurationInNanos
        numPeriods * numTokensPerPeriod
      }
    }
  }
}

