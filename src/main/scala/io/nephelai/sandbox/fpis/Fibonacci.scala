package io.nephelai.sandbox.fpis

import scala.math.BigInt

/**
 * Created by teddyphreak on 9/17/15.
 */
object Fibonacci {

  def fib(n: Int): Int = {
    if (n == 0)
      0
    else if (n == 1)
      1
    else
      fib(n - 2) + fib(n - 1)
  }
}
