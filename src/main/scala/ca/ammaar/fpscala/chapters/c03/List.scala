package ca.ammaar.fpscala.chapters.c03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

  def product(doubles: List[Double]): Double =
    doubles match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(d, ds) => d * product(ds)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a: List[A], b: List[A]): List[A] =
    a match {
      case Nil => b
      case Cons(h, t) => Cons(h, append(t, b))
    }

  /**
   * Exercise 3.2
   */
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }

  /**
   * Exercise 3.3
   */
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(_, t) => Cons(h, t)
    }

  /**
   * Exercise 3.4
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 1) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  /**
   * Exercise 3.5
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else l
    }

  /**
   * Exercise 3.6
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
}
