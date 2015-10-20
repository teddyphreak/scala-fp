package io.nephelai.test.sandbox.fips

import io.nephelai.sandbox.fpis.Fibonacci.fib
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.{Gen, Properties}

/**
 * Created by teddyphreak on 9/17/15.
 */
object FibonacciSpec extends Properties("Fibonacci") {

  property("fib(0)") = { fib(0) == 0 }

  property("fib(1)") = { fib(1) == 1 }

  property("fib(n)") = forAll(Gen.choose(2, 30)) { (n:Int) => (n >= 2 && n <= 10) ==> (fib(n) == fib(n - 1) + fib(n - 2)) }

}
