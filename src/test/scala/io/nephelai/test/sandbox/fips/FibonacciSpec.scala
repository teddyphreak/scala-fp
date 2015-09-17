package io.nephelai.test.sandbox.fips

import io.nephelai.sandbox.fpis.Fibonacci

/**
 * Created by teddyphreak on 9/17/15.
 */
object FibonacciSpec extends org.specs2.mutable.Specification {

  "Fibonacci specification" >> {
    "fib(0) must be 0" >> {
      Fibonacci.fib(0) must_== 0
    }
    "fib(1) must be 1" >> {
      Fibonacci.fib(1) must_== 1
    }
    "fib(n) must be fib(n-1) + fib(n-2)" >> {
      Fibonacci.fib(5) must_== Fibonacci.fib(4) + Fibonacci.fib(3)
    }

  }

}
