package io.nephelai.test.sandbox.fips

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen.{posNum, nonEmptyListOf}
import io.nephelai.sandbox.fpis.List.{isSorted, tail, setHead, dropWhile, init, foldRight, length, sum, product, reverse, append, flatten, add, filter, zipWith, hasSubsequence }

/**
 * Created by teddyphreak on 9/26/15.
 */
object ListsSpec extends Properties("List") {

  property("isSorted(Nil)") = isSorted[Int](Nil, (a, b) => a <= b) == true

  property("isSorted(x :: Nil)") = forAll { (n: Int) => isSorted(n :: Nil, (a: Int, b: Int) => a <= b) }

  property("isSorted(x :: tail)") = forAll { (l: List[Int]) => (l.size > 2) ==> isSorted(l.sorted, (a: Int, b: Int) => a <= b)  }

  property("isSorted(x :: tail)") = forAll { (l: List[Int]) => (l.size > 2) ==> ! isSorted(l.sorted, (a: Int, b: Int) => a > b) }

  property("tail(l)") = forAll(nonEmptyListOf(posNum[Int])) { (l: List[Int]) => tail(l) == l.tail }

  property("setHead(h, l)") = forAll(posNum[Int], nonEmptyListOf(posNum[Int])) { (n: Int, l: List[Int]) => setHead(n, l) == n :: l.tail }

  property("dropWhile(_ => false, l)") = forAll { (l: List[Int], f: Int => Boolean) => dropWhile(f, l) == l.dropWhile(f) }

  property("init(l)") = forAll(nonEmptyListOf(posNum[Int]), posNum[Int]) { (l: List[Int], n: Int) => init((n :: l.reverse).reverse) == l }

  property("foldRight(l, 0)(_ + _)") = forAll { (l: List[Int], n: Int) => foldRight(l, n)(_ + _) == l.sum + n }

  property("length(l)") = forAll { (l: List[Int]) => length(l) == l.size }

  property("sum(l") = forAll { (l: List[Int]) => sum(l) == l.sum }

  property("product(l") = forAll { (l: List[Int]) => product(l) == l.product }

  property("reverse(l)") = forAll { (l: List[Int]) => reverse(l) == l.reverse }

  property("append(l") = forAll { (l: List[Int], n: Int) => append(l, n) == (n :: l.reverse).reverse }

  property("flatten(l)") = forAll { (l: List[List[Int]]) => flatten(l) == l.flatten }

  property("add(l, n)") = forAll { (l: List[Int], n: Int) => add(l, n) == l.map(_ + n) }

  property("toString(l)") = forAll { (l: List[Int]) => io.nephelai.sandbox.fpis.List.toString(l) == l.map(_.toString()) }

  property("filter(l, f)") = forAll { (l: List[Int], f: Int => Boolean) => filter(l, f) == l.filter(f(_)) }

  property("flatMap(l)(f)") = forAll { (l: List[Int], f: Int => List[Int]) => io.nephelai.sandbox.fpis.List.flatMap(l)(f) == l.flatMap(f) }

  property("zipWith(l)(f)") = forAll { (a: List[Int], b: List[Int], f: (Int, Int) => Int) => zipWith(a, b, f) == a.zip(b).map(x => f(x._1, x._2)) }

  property("hasSubSequence(l, f)") = hasSubsequence(Nil, Nil) == false
  property("hasSubSequence(l, f)") = hasSubsequence(Nil, List(1, 2, 3, 4, 5, 6)) == false
  property("hasSubSequence(l, f)") = hasSubsequence(List(1, 2, 3, 4, 5, 6), Nil) == true
  property("hasSubSequence(l, f)") = hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4)) == true
  property("hasSubSequence(l, f)") = hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 2)) == false
  property("hasSubSequence(l, f)") = hasSubsequence(List(1, 2, 3, 4, 5, 6), List(7, 8)) == false
  property("hasSubSequence(l, f)") = hasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 3, 4, 5, 6)) == true

}