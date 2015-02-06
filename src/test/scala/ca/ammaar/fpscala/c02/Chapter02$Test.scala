package ca.ammaar.fpscala.c02

import org.scalatest.FunSuite

import scala.util.Try

import Chapter02._

class Chapter02$Test extends FunSuite {
  import Chapter02$Test._

  /**
   * Exercise 2.1 Tests
   */

  test("First 10 values in the fibonacci sequence") {
    val list: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val fibList: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

    assert(list.map(fib).equals(fibList))
  }

  test("Last valid value for Int before overflow") {
    assert(fib(46) == 1836311903)
  }

  test("Overflow at n == 47") {
    assert(fib(47) == -1323752223)
  }

  test("Negative n value throws an error") {
    assert(Try(fib(-1)).isFailure)
  }


  /**
   * Exercise 2.2 Tests
   */

  test("Empty array is ordered") {
    val as: Array[Int] = Array[Int]()
    assert(isSorted(as, ascending))
  }

  test("Array with one item is ordered") {
    val as: Array[Int] = Array[Int](1)
    assert(isSorted(as, ascending))
  }

  test("Array with two items is ordered") {
    val as: Array[Int] = Array[Int](0, 1)
    assert(isSorted(as, ascending))
  }

  test("Array with two items is not ordered") {
    val as: Array[Int] = Array[Int](1, 0)
    assert(!isSorted(as, ascending))
  }

  test("Array with many items is ordered") {
    val as: Array[Int] = Array[Int](-1, 0, 1, 2)
    assert(isSorted(as, ascending))
  }

  test("Array with last item out of order") {
    val as: Array[Int] = Array[Int](0, 1, 2, 1)
    assert(!isSorted(as, ascending))
  }

  test("Array with first item out of order") {
    val as: Array[Int] = Array[Int](1, 0, 2, 3)
    assert(!isSorted(as, ascending))
  }

  test("Array with items in between out of order") {
    val as: Array[Int] = Array[Int](0, 1, 3, 2, 4)
    assert(!isSorted(as, ascending))
  }

}

object Chapter02$Test {

  /**
   * Exercise 2.2 Helper function
   *
   * @param a The first Int
   * @param b The second Int
   * @return `true` if a is less than or equal to b
   */
  def ascending(a: Int, b: Int): Boolean = a <= b
}
