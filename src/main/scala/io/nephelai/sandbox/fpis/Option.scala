package io.nephelai.sandbox.fpis

/**
  * Created by teodo on 11/28/2015.
  */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None()
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None())

  def getOrElse[B >: A](default: => B): B = this match {
    case None() => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None()
  }

}

case class Some[A](x: A) extends Option[A]
case class None[A]() extends Option[A]