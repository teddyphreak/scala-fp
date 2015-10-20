package io.nephelai.test.sandbox.fips

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}
import io.nephelai.sandbox.fpis.Compose.compose

/**
 * Created by teddyphreak on 9/26/15.
 */
object ComposeSpec extends Properties("Compose") {

  property("compose(f, g)") = forAll { (f: String => Int, g: Int => String, x: String) => compose(f, g)(x) == g(f(x))}

  property("compose(f, g)") = forAll { (f: Int => String, g: String=> Int, x: Int) => compose(f, g)(x) == g(f(x))}

}
