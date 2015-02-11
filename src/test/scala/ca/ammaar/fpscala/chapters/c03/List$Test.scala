package ca.ammaar.fpscala.chapters.c03

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class List$Test extends FunSuite {
  import List._

  val l: List[Int] = List(1, 2, 3)

  test("tail returns the tail of a list") {
    assert(tail(l).equals(List(2, 3)))
    assert(tail(tail(l)).equals(List(3)))
    assert(tail(tail(tail(l))).equals(Nil))
  }

  test("setHead changes the head of a list") {
    assert(setHead(l, 0).equals(List(0, 2, 3)))
  }

  test("drop removes the head of a list") {
    assert(drop(l, 1).equals(List(2, 3)))
    assert(drop(l, 2).equals(List(3)))
    assert(drop(l, 3).equals(Nil))
  }

  test("dropWhile removes elements while some condition is true") {
    def cond(s: Int): (Int) => Boolean = { i: Int => i < s }
    assert(dropWhile(l, cond(1)).equals(List(1, 2, 3)))
    assert(dropWhile(l, cond(2)).equals(List(2, 3)))
    assert(dropWhile(l, cond(3)).equals(List(3)))
    assert(dropWhile(l, cond(4)).equals(Nil))
  }

  test("dropWhileCurried removes elements while some condition is true") {
    assert(dropWhileCurried(l)(_ < 1).equals(List(1, 2, 3)))
    assert(dropWhileCurried(l)(_ < 2).equals(List(2, 3)))
    assert(dropWhileCurried(l)(_ < 3).equals(List(3)))
    assert(dropWhileCurried(l)(_ < 4).equals(Nil))
  }

  test("init returns all but the last element") {
    assert(init(l).equals(List(1, 2)))
    assert(init(init(l)).equals(List(1)))
    assert(init(init(init(l))).equals(Nil))
  }

  test("length") {
    assert(length(Nil) == 0)
    assert(length(l) == 3)
  }

  test("length2") {
    assert(length2(Nil) == 0)
    assert(length2(l) == 3)
  }

  test("Reverse a list") {
    assert(reverse(Nil).equals(Nil))
    assert(reverse(l).equals(List(3, 2, 1)))
    assert(reverse(List(1)).equals(List(1)))
    assert(reverse(List(1, 2)).equals(List(2, 1)))
  }

  test("concatenate some lists") {
    assert(concat(List(Nil, Nil)).equals(Nil))
    assert(concat(List(l, Nil)).equals(List(1, 2, 3)))
    assert(concat(List(l, l)).equals(List(1, 2, 3, 1, 2, 3)))
  }
}
