package io.nephelai.test.sandbox.fips

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import io.nephelai.sandbox.fpis.Curry.{curry, uncurry }

/**
 * Created by teddyphreak on 9/26/15.
 */
object CurrySpec extends Properties("Curry") {

  property("curry(uncurry)") = forAll { (f: (Int, Int) => Int, a: Int, b: Int) => uncurry(curry(f))(a, b) == f(a, b) }

  property("curry(f)(a)(b)") = forAll { (f: (Int, Int) => Int, a: Int, b: Int) => curry(f)(a)(b) == f(a, b) }

  property("uncurry(f)(a, b)") = forAll { (f: (Int => (Int => Int)), a: Int, b: Int) => uncurry(f)(a, b) == f(a)(b) }

}
