package io.nephelai.sandbox.fpis

/**
 * Created by teddyphreak on 10/16/15.
 */
abstract sealed class Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {


  def size[A](t: Tree[A]): Int = fold(t)(
    v => 1
  )(
    (l, r) => 1 + size(l) + size(r)
  )


  def max[A](t: Tree[A])(implicit n: Numeric[A]): A = fold(t)(
    identity
  )(
    (l, r) => n.max(max(l), max(r))
  )


  def depth[A](t: Tree[A]): Int = fold(t)(
    v => 1
  )(
    (l, r) => 1 + depth(l) max depth(r)
  )

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(
    v => Leaf(f(v))
)(
    (l, r) => Branch(map(l)(f), map(r)(f))
  )

  def fold[A, B](t: Tree[A])(f: A => B)(g: (Tree[A], Tree[A]) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(l, r)
  }

}
