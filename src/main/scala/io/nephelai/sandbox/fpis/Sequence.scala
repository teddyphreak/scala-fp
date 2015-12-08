package io.nephelai.sandbox.fpis

import io.nephelai.sandbox.fpis.Option.map2

/**
  * Created by teddyphreak on 12/2/2015.
  */
object Sequence {

  def sequence[A](a: Seq[Option[A]]): Option[Seq[A]] = a.foldRight(Some(Nil): Option[Seq[A]])((a, b) => map2(a, b)( (x, y) => x +: y))

}


