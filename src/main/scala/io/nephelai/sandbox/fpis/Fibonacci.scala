package io.nephelai.sandbox.fpis

import scala.math.BigInt

/**
 * Created by teddyphreak on 9/17/15.
 */
object Fibonacci {

  def fib(n: Int):Int = {
    //TODO: Make function tail recursive (defer to helper generator for Fibonacci sequence and get last number)
    assert(n >= 0)
    if ((n == 0) || (n == 1))
      n
    else
      fib(n - 2) + fib(n - 1)
  }
}
