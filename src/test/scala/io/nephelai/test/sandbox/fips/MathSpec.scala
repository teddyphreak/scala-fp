package io.nephelai.test.sandbox.fips

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import io.nephelai.sandbox.fpis.Math.{mean, variance}
import io.nephelai.sandbox.fpis.{Some => Algo, None => Nada}

import scala.collection.immutable.Stream.Empty

/**
  * Created by teddyphreak on 11/28/2015.
  */
object MathSpec extends Properties("Math") {

  property("mean(x") = forAll { (x: List[Double]) =>
    x match {
      case Nil => mean(x) == Nada()
      case l => mean(l) == Algo(l.sum / l.length)
    }
  }

  property("variance(x)") = forAll { (a: List[Double]) =>
    a match {
      case Nil => variance(a) == Nada()
      case l => variance(a) == Algo(l.map(x => math.pow(x - (l.sum / l.length), 2)).sum)
    }
  }

}
