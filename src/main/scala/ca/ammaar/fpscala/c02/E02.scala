package ca.ammaar.fpscala.c02

object E02 {

  /**
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

}
