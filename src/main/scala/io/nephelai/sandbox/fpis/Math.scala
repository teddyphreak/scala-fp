package io.nephelai.sandbox.fpis

import io.nephelai.sandbox.fpis.{Option => Opcion, Some => Algo, None => Nada}
import io.nephelai.sandbox.fpis.Curry.{curry, uncurry}

import scala.collection.immutable.Stream.Empty

/**
  * Created by teodo on 11/28/2015.
  */
object Math {

  def mean(xs: Seq[Double]): Opcion[Double] = xs match {
    case Nil => Nada()
    case s => Algo(s.sum / s.length)
  }

  def sum(a: Opcion[Double], b: Opcion[Double]): Opcion[Double] = a.flatMap(x => b.map(y => x + y))

  def variance(xs: Seq[Double]): Opcion[Double] = xs match {
    case Nil => Nada()
    case s => mean(s) match {
      case n @ Nada() => n
      case Algo(m) => Algo(xs.map(x => x - m).map(x => math.pow(x, 2)).foldLeft(0.0)(_ + _))
    }
  }
}