package ca.ammaar.fpscala.chapters.c03

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class List$Test extends FunSuite with BeforeAndAfterEach {
  import List._

  implicit var l: List[Int] = _

  override def beforeEach(): Unit = l = List(1, 2, 3)

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

  test("init returns all but the last element") {
    assert(init(l).equals(List(1, 2)))
    assert(init(init(l)).equals(List(1)))
    assert(init(init(init(l))).equals(Nil))
  }
}
