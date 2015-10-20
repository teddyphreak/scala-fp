package io.nephelai.sandbox.fpis

/**
 * Created by teddyphreak on 9/26/15.
 */
object Compose {

  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

}
