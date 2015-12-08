package io.nephelai.test.sandbox.fips

import io.nephelai.sandbox.fpis.{Some => Algo, None => Nada, Option => Opcion}
import org.scalacheck.{Gen, Arbitrary, Prop, Properties}
import io.nephelai.sandbox.fpis.Sequence.{sequence, traverse}
import io.nephelai.sandbox.fpis.{Option, Some, None}

import scala.util.Random

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
  implicit def arbitraryOption = Arbitrary[Opcion[Int]](genOption)

  property("sequence(l)") = Prop.forAll { s: List[Algo[Int]] =>
    val nullSeq: Seq[Opcion[Int]] = s match {
      case Nil => Nada() +: s
      case head :: tail => head +: Nada() +: tail
    }
    sequence(nullSeq) == Nada()
    sequence(s) == Algo(s.map { _ match { case Algo(y) => y } })
  }

  property("traverse(l)(f)") = Prop.forAll { s: List[Int] =>
    s match {
      case Nil => traverse(s)(Algo(_)) == Algo(Nil)
      case head :: tail => 
        traverse(s)(Algo(_)) == Algo(s)
        traverse(s)(_ => Nada()) == Nada()
    }
  }

}
