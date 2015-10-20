package io.nephelai.sandbox.fpis

/**
 * Created by teddyphreak on 9/26/15.
 */
object Curry {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => (b => f(a, b))

  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)

}