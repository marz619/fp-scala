package ca.ammaar.fpscala.c02

object E01 {

  /**
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
    def go(n: Int, prev: Int, acc: Int): Int = {
      if (n == 0) acc
      else go(n - 1, acc, acc + prev)
    }

    go(n, 1, 0)
  }
}
