package ca.ammaar.fpscala.c02

import org.scalatest._
import E01.fib

import scala.util.Try

class E01$Test extends FunSuite {

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
}