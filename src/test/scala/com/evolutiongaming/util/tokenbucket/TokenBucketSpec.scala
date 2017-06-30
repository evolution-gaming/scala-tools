package com.evolutiongaming.util.tokenbucket

import org.scalatest.{FlatSpec, Matchers}

class TokenBucketSpec extends FlatSpec with Matchers {

  "TokenBucket" should "block subsequent calls which exceed the rate per period" in new Scope {

    for (_ <- 1L to bucket.ratePerPeriod) {
      bucket.tryConsume() shouldBe true
    }

    for (_ <- 1L to bucket.ratePerPeriod) {
      bucket.tryConsume() shouldBe false
    }
  }

  it should "allow to consume specified number of tokens and refill manually" in new Scope {

    for (_ <- 1L to bucket.ratePerPeriod - 2) {
      bucket.tryConsume() shouldBe true
    }

    bucket tryConsume 3 shouldBe false
    bucket tryConsume 2 shouldBe true

    bucket refill 2

    bucket tryConsume 3 shouldBe false
    bucket.tryConsume() shouldBe true
    bucket.tryConsume() shouldBe true
    bucket.tryConsume() shouldBe false
  }

  it should "refill the bucket automatically at the end of a period" in new Scope {

    for (_ <- 1L to bucket.ratePerPeriod) {
      bucket.tryConsume() shouldBe true
    }

    bucket.tryConsume() shouldBe false

    Thread sleep bucket.periodInMillis

    for (_ <- 1L to bucket.ratePerPeriod) {
      bucket.tryConsume() shouldBe true
    }

    bucket.tryConsume() shouldBe false
  }

  it should "don't allow to refill to more than specified rate per period" in {

    val bucket = TokenBucket(ratePerPeriod = 2)

    bucket.tryConsume() shouldBe true
    bucket.tryConsume() shouldBe true
    bucket.tryConsume() shouldBe false

    bucket refill 5

    bucket.tryConsume() shouldBe true
    bucket.tryConsume() shouldBe true
    bucket.tryConsume() shouldBe false
  }

  private trait Scope {
    val bucket = TokenBucket(ratePerPeriod = 10, periodInMillis = 500L)
  }
}
