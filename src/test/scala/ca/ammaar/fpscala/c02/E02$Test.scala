package ca.ammaar.fpscala.c02

import org.scalatest.FunSuite
import E02.isSorted

class E02$Test extends FunSuite {

  def ascending(a: Int, b: Int): Boolean = a <= b

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
