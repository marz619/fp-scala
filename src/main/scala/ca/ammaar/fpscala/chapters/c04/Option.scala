package ca.ammaar.fpscala.chapters.c04

import scala.{Option => _, Either => _} // hides the standard library implementations

sealed trait Option[+A] {

  def get: A
  def isEmpty: Boolean

  def isDefined: Boolean = this match {
    case None => false
    case Some(x) => true
  }

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

final case class Some[+A](get: A) extends Option[A] {
  def isEmpty = false
}

case object None extends Option[Nothing] {
  def isEmpty = true
  def get = throw new NoSuchElementException("None.get")
}

object Option {
  /**
   * Calculates the mean of a sequence of Doubles
   */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
   * Calculates the variance of a sequence of Doubles
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

}


