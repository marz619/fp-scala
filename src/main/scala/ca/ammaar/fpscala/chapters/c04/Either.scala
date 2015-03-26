package ca.ammaar.fpscala.chapters.c04

import scala.{Either => _} // hides the standard library implementation

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      eb <- b
    } yield f(a, eb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def saveDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def safeDiv_1(x: Int, y: Int): Either[Exception, Int] = Try(x / y)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  /**
   * Exercise 4.7
   * sequence: Turns a List of Either(s) into an Either of List
   * traverse: Maps a list of A onto an Either of list of B
   */

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h.flatMap(rh => sequence(t).map(rh :: _))
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).flatMap(rh => traverse(t)(f).map(rh :: _))
    }

  /**
   * Exercise 4.8
   *
   * How to accumulate a list of Exceptions?
   *
   * We could use Either[List[E], _] type, with modified map2 and sequence functions.
   *
   * OR
   *
   * We can create a new data type that accumulates a list of Errors in the
   * constructor. This implementation 'Validates' a series of initial parameters
   * and accumulates any errors.
   *
   * trait Validates[+E, +A]
   * case class Errors[+E](get: Seq[E]) extends Validates[E, Nothing]
   * case class Success[+A](get: A) extends Validates[Nothing, A]
   *
   * we can implement map, map2, and sequence for this Validates type, however flatMap would not work
   * due to its nature of failing fast
   */

}