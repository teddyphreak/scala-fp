package io.nephelai.test.sandbox.fips

import io.nephelai.sandbox.fpis.{Some, None}
import io.nephelai.sandbox.fpis.Option.map2
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

/**
  * Created by teddyphreak on 11/28/2015.
  */
object OptionSpec extends Properties("Option") {

  val genSome = for {
    n <- Arbitrary.arbitrary[Int]
  } yield Some(n)
  implicit def arbitrarySome = Arbitrary(genSome)
  val genNone = Gen.const(None())
  implicit def arbitraryNone = Arbitrary(genNone)

  property("map(f)") = forAll { (f: Int => Int, x: Int) =>
    None().map(f) == None()
    Some(x).map(f) == Some(f(x))
  }

  property("flatMap(f)") = forAll { (f: Int => Some[Int], x: Int) =>
    None().flatMap(f) == None()
    Some(x).flatMap(f) == f(x)
  }

  property("getOrElse(x)") = forAll { (x: Int, y: Int) =>
    None().getOrElse(y) == y
    Some(x).getOrElse(y) == x
  }

  property("orElse(x)") = forAll { (x: Int, y: Int) =>
    None().orElse(Some(x)) == Some(x)
    None().orElse(None()) == None()
    Some(x).orElse(Some(y)) == Some(x)
    Some(x).orElse(None()) == Some(x)
  }

  property("filter(f)") = forAll { (x: Int, f: (Int => Boolean)) =>
    f(x) match {
      case true => Some(x).filter(f) == Some(x)
      case false => Some(x).filter(f) == None()
    }
    None().filter(f) == None()
  }

  property("map2(a, b)(f)") = forAll { (x: Int, y: Double, f: (Int, Double) => Double) =>
    map2(None(), None())(f) == None()
    map2(None(), Some(y))(f) == None()
    map2(Some(x), None())(f) == None()
    map2(Some(x), Some(y))(f) == Some(f(x, y))
  }
}
