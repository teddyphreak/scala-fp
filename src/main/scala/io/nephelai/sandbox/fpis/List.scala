package io.nephelai.sandbox.fpis

/**
 * Created by teddyphreak on 9/26/15.
 */
object List {

  def isSorted[A](c: Iterable[A], f: (A, A) => Boolean): Boolean = c match {
    case Nil => true
    case x :: Nil => true
    case x :: tail => f(x, tail.head) && isSorted(tail, f)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case _ :: t => t
  }

  def setHead[A](h: A, l: List[A]): List[A] = l match {
    case Nil =>
      h :: Nil
    case _ :: t =>
      h :: t
  }

  def drop[A](n: Int, l: List[A]): List[A] = (n, l) match {
    case (x, xs) if x == 0 =>
      xs
    case (x, xs) if x > 0 =>
      xs match {
        case Nil => Nil
        case _ :: t => drop(n - 1, t)
      }
  }

  def drop[A](l: List[A], n: Int): List[A] = drop(n, l)

  def dropWhile[A](f: A => Boolean, l: List[A]): List[A] = l match {
    case Nil => Nil
    case h :: t => if (f(h)) { dropWhile(f, t) } else { h :: t }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = dropWhile(f, l)

  def init[A](l: List[A]): List[A] = l match {
    case h :: t => t match {
      case Nil => Nil
      case _ => h :: init(t)
    }
    case Nil => Nil
  }

  def foldRight[A,B](xs: Iterable[A], z: B)(f: (A, B) => B): B =
    foldLeft(xs, identity[B](_))((g, x) => w => g(f(x, w)))(z)

  def foldLeft[A,B](xs: Iterable[A], z: B)(f: (B, A) => B): B =
    xs match {
      case Nil => z
      case h :: t => foldLeft(t, f(z, h))(f)
    }

  def length[A](as: List[A]): Int = foldRight(as, 0)((x, t) => 1 + t)

  def sum[A](as: List[A])(implicit numeric: Numeric[A]): A = foldLeft(as, numeric.zero)(numeric.plus(_, _))

  def product[A](as: List[A])(implicit numeric: Numeric[A]): A = foldLeft(as, numeric.one)(numeric.times(_, _))

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((xs: List[A], x: A) => x :: xs )

  def append[A](as: List[A], x: A): List[A] = foldRight(as, x :: Nil)((z: A, y: List[A]) => z :: y)

  def flatten[A](as: List[List[A]]): List[A] = foldLeft(as, Nil:List[A])((xc: List[A], xs: List[A]) => foldLeft(xs , xc)((y: List[A], z: A) => append(y, z)))

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((x, xs) => f(x) :: xs)

  def add[A](as: List[A], n: A)(implicit numeric: Numeric[A]): List[A] = map(as)(numeric.plus(_, n))

  def toString[A](as: List[A]): List[String] = map(as)(_.toString())

  def filter[A](as: List[A], f: A => Boolean) = {
    flatMap(as)(x => f(x) match {
      case true => x :: Nil
      case default => Nil
    })
  }

  // O(N) substitute for trivial O(N^2) implementation flatten(map(as, f))
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])(
      (x, xs) => foldRight(f(x), xs)(
        (y, ys) => y :: ys
      ))

  def zipWith[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List [C] =
    (as, bs) match {
      case (xa :: Nil, xb :: Nil) => f(xa, xb) :: Nil
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (xa :: ta, xb :: tb) => f(xa, xb) :: zipWith(ta, tb, f)
    }
  
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def beginsWith(l: List[A], p: List[A]): Boolean = (l, p) match {
      case (lh :: lt, ph :: pt) if (lh == ph) => beginsWith(lt, pt)
      case (_, Nil) => true
      case default => false
    }
    (l, sub) match {
      case (h :: t, s) => beginsWith(l, sub) || hasSubsequence(l.tail, sub)
      case default => false
    }
  }
}
