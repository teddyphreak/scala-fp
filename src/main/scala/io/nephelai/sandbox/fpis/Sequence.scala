package io.nephelai.sandbox.fpis

import io.nephelai.sandbox.fpis.{Option => Opcion, Some => Algo, None => Nada}

/**
  * Created by teddyphreak on 12/2/2015.
  */
object Sequence {

  def sequence[A](a: Seq[Opcion[A]]): Opcion[Seq[A]] = traverse(a)(identity)

  def traverse[A, B](a: Seq[A])(f: A => Opcion[B]): Opcion[Seq[B]] = {
    a.foldRight(Algo(Nil): Opcion[Seq[B]])((x, y) =>
      (f(x), y) match {
        case (Algo(i), Algo(l)) => Algo(i +: l)
        case _ => Nada()
      }
    )
  }

}