package ca.ammaar.fpscala.chapters.c04

import scala.{Option => _} // hides the standard library implementation

sealed trait Option[+A] {

  def get: A
  def isEmpty: Boolean
  def isDefined: Boolean

  /**
   * returns Some[B] if this[A] via f
   */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /**
   * returns default if this is None, other
   */
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  /**
   * Tries to convert this via f into an Option[B] or
   * None
   */
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  /**
   * if this is Some(A), return it, otherwise return Some(ob),
   * None otherwise
   */
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  /**
   * return an Option if some value a satisfies f,
   * None otherwise
   */
  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}

final case class Some[+A](get: A) extends Option[A] {
  def isEmpty = false
  def isDefined = true
}

case object None extends Option[Nothing] {
  def isEmpty = true
  def isDefined = false
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
   * Exercise 4.2 Calculates the variance of a sequence of Doubles
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
   * Lift a function to be Option'd
   */
  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  def abs0: Option[Double] => Option[Double] = lift(math.abs)

  /**
   * Try function
   */
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  /**
   * Exercise 4.3 map2, that combines two Option values
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(sa => b.map(sb => f(sa, sb)))

  /**
   * Exercise 4.4 sequence
   *
   * Takes a List of Option(s) and via Option.flatMap
   * converts it into an Option of List
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(sh => sequence(t).map(sh :: _))
    }

  /**
   * Exercise 4.5 traverse
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(sh => traverse(t)(f).map(sh :: _))
    }

  /**
   * Exercise 4.5 sequence via traverse
   */
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(_.map(b => b))

}


