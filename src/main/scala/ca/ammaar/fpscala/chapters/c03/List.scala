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

  def append[A](a: List[A], b: List[A]): List[A] = a match {
    case Cons(h, t) => Cons(h, append(t, b))
    case Nil => b
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def product2(doubles: List[Double]): Double =
    foldRight(doubles, 1.0)(_ * _)

  /**
   * Exercise 3.2
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  /**
   * Exercise 3.3
   */
  def setHead[A](l: List[A], h: A): List[A] = l match {
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
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => as
  }

  /**
   * This way is better for type inference
   *
   * i.e. we don't have to define the type in the matching function
   *
   * <pre>
   *   val xs: List(1, 2, 3, 4)
   *   val as = dropWhile(xs)(x => x < 4)
   *   // val as = dropWhile(xs)(_ < 4)
   * </pre>
   */
  def dropWhileCurried[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => dropWhileCurried(t)(f)
    case _ => as
  }

  /**
   * Exercise 3.6
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
   * Exercise 3.9
   */
  def length[A](l: List[A]): Int =
    foldRight[A, Int](l, 0)((_, acc) => acc + 1)

  /**
   * Exercise 3.10
   */
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
   * Exercise 3.11
   */
  def sum3(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product3(doubles: List[Double]): Double = foldLeft(doubles, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  /**
   * Exercise 3.12
   */
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((t, h) => Cons(h, t))
  }

  /**
   * Exercise 3.13
   */
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  /**
   * Exercise 3.14
   */
  def appendViaFoldRight[A](a: List[A], b: List[A]): List[A] =
    foldRight(a, b)(Cons(_, _))

  /**
   * Exercise 3.15
   */
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil:List[A])(append)
}
