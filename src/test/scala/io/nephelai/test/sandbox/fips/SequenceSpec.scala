package io.nephelai.test.sandbox.fips

import io.nephelai.sandbox.fpis.{Some => Algo, None => Nada, Option => Opcion}
import org.scalacheck.{Gen, Arbitrary, Prop, Properties}
import io.nephelai.sandbox.fpis.Sequence.sequence
import io.nephelai.sandbox.fpis.{Option, Some, None}

/**
  * Created by teddyphreak on 12/2/2015.
  */
object SequenceSpec extends Properties("Sequence") {

  val genSome = for {
    n <- Arbitrary.arbitrary[Int]
  } yield Algo(n)
  implicit def arbitrarySome = Arbitrary(genSome)
  val genNone = Gen.const(Nada())
  implicit def arbitraryNone = Arbitrary(genNone)
  val genOption = Gen.oneOf(genNone, genSome)
  implicit def arbitraryOption = Arbitrary[Option[Int]](genOption)

  property("sequence(l)") = Prop.forAll { (l: Seq[Opcion[Int]], s: Seq[Int]) =>
    l.contains(Nada()) match {
      case true => sequence(l) == Nada()
      case false => sequence(s.map(x => Algo(x))) == Algo(s)
    }
  }

}
