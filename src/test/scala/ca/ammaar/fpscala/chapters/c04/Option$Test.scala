package ca.ammaar.fpscala.chapters.c04

import org.scalatest.FunSuite

import Option._
import ca.ammaar.fpscala.chapters.util._

class Option$Test extends FunSuite {

  val l: List[Double] = List(1.0, 2.0, 3.0, 4.0)
  val ol: List[Option[Double]] = List(1.0, 2.0, 3.0, 4.0).map(Some(_))

  test("The mean of a sequence") {
    assert(mean(l).get ~= 2.5)
  }

  test("The variance of a sequence") {
    assert(variance(l).get ~= 1.25)
  }

  test("Sequence and Sequence Via Traverse") {
    assert(sequence(List()).equals(Some(List())))
    assert(sequence(ol).equals(Some(l)))
    assert(sequenceViaTraverse(List()).equals(Some(List())))
    assert(sequenceViaTraverse(ol).equals(Some(l)))
  }
}
