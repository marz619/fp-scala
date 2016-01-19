package ca.ammaar.fpscala.chapters.c03

import org.scalatest.FunSuite

class List$Test extends FunSuite {
  import List._

  val l: List[Int] = List(1, 2, 3)

  test("exercise 3.1") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  test("tail returns the tail of a list") {
    assert(tail(l).equals(List(2, 3)))
    assert(tail(tail(l)).equals(List(3)))
    assert(tail(tail(tail(l))).equals(Nil))
  }

  test("setHead changes the head of a list") {
    assert(setHead(l, 0).equals(List(0, 2, 3)))
    assert(setHead(Nil, 42).equals(List(42, Nil)))
  }

  test("drop removes the head of a list") {
    assert(drop(l, 1).equals(List(2, 3)))
    assert(drop(l, 2).equals(List(3)))
    assert(drop(l, 3).equals(Nil))
  }

  test("dropWhile removes elements while some condition is true") {
    def cond(s: Int): (Int) => Boolean = _ < s
//    def cond(s: Int): (Int) => Boolean = { i: Int => i < s }

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

  test("plusOne returns new list with elements incremented by one") {
    assert(plusOne(Nil).equals(Nil))
    assert(plusOne(l).equals(List(2, 3, 4)))
  }
  
  test("doubleToString converts List[Double] to List[String]") {
    assert(doubleToString(Nil).equals(Nil))
    assert(doubleToString(List(1.0, 2.0, 3.0)).equals(List("1.0", "2.0", "3.0")))
  }

  test("map some things") {
    assert(map(Nil)((a) => a).equals(Nil))
    assert(map(l)((a) => a).equals(l))
    assert(map(l)(_.toString).equals(List("1", "2", "3")))
  }

  test("test filter") {
    assert(filter(Nil)(_ => true).equals(Nil))
    assert(filter(l)(_ => true).equals(List(1, 2, 3)))
    assert(filter(l)(_ % 2 == 0).equals(List(2)))
  }

  test("test filterNaive") {
    assert(filterNaive[Nothing](Nil, _ => true).equals(Nil))
    assert(filterNaive[Int](l, _ => true).equals(List(1, 2, 3)))
    assert(filterNaive[Int](l, (x: Int) => x % 2 == 0).equals(List(2)))
  }

  test("test flatMap") {
    assert(flatMap(Nil)((a) => List(a)).equals(Nil))
    assert(flatMap(l)((i) => List(i, i)).equals(List(1,1,2,2,3,3)))
  }

  test("test filter via flatMap") {
    assert(filterViaFlatMap(Nil)(_ => true).equals(Nil))
    assert(filterViaFlatMap(l)(_ => true).equals(List(1, 2, 3)))
    assert(filterViaFlatMap(l)(_ % 2 == 0).equals(List(2)))
  }

  test("test has sub sequence") {
    assert(hasSubSequence(Nil, Nil))
    assert(hasSubSequence(l, Nil))
    assert(hasSubSequence(l, List(1)))
    assert(hasSubSequence(l, List(2)))
    assert(hasSubSequence(l, List(3)))
    assert(hasSubSequence(l, List(1, 2)))
    assert(hasSubSequence(l, List(2, 3)))
    assert(hasSubSequence(l, l))
  }
}
