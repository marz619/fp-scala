package ca.ammaar.fpscala.c2

object E1 {

  /**
   * Compute the nth number in the fibonacci sequence
   * @param n the nth number
   * @return the nth number in the fibonacci sequence
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
