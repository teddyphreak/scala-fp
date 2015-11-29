package io.nephelai.sandbox.fpis

import io.nephelai.sandbox.fpis.{Option, Some, None}
import io.nephelai.sandbox.fpis.Curry.{curry, uncurry}

import scala.collection.immutable.Stream.Empty

/**
  * Created by teodo on 11/28/2015.
  */
object Math {

  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None()
    case s => Some(s.reduce(_ + _) / s.length)
  }

  def sum(a: Option[Double], b: Option[Double]): Option[Double] = a.flatMap(x => b.map(y => x + y))

  def variance(xs: Seq[Double]): Option[Double] = xs match {
    case Nil => None()
    case s => ------------------------------------------------------------------------------------xs.map(x => m.map(y => math.pow(x - y, 2))).foldLeft(Some(0.0): Option[Double])(sum)
  }
}