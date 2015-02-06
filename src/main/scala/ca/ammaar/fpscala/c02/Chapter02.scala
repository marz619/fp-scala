package ca.ammaar.fpscala.c02

object Chapter02 {

  /**
   * Exercise 2.1
   *
   * Compute the nth number in the fibonacci sequence.
   * The sequence beginning with 0 and 1, is such that
   * each subsequent number in the sequence is the sum
   * of the two previous numbers.
   *
   * i.e. 0, 1, 1, 2, 3, 5, ...
   *
   * @param n The nth number to compute.
   * @return The nth number in the fibonacci sequence
   */
  def fib(n: Int): Int = {
    require(n >= 0)

    @annotation.tailrec
    def loop(n: Int, prev: Int, acc: Int): Int = {
      if (n == 0) acc
      else loop(n - 1, acc, acc + prev)
    }

    loop(n, 1, 0)
  }

  /**
   * Exercise 2.2
   *
   * Given an array and an ordering function, return whether the array
   * is ordered.
   *
   * @param as The array to check
   * @param ordered predicate which accepts two members of as and deterines
   *                if they are ordered
   * @tparam A The type parameter of this function
   * @return `true` if `as` is ordered, `false` otherwise
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (as.length <= 1 || n == as.length) true
      else if (!ordered(as(n-1), as(n))) false
      else loop(n + 1)
    }
    loop(1)
  }


  /**
   * Exercise 2.3
   */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   * Exercise 2.4
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * Exercise 2.5
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
