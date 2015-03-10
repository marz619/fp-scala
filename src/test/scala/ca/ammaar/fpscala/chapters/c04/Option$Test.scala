package ca.ammaar.fpscala.chapters.c04

import org.scalatest.FunSuite

import Option._
import ca.ammaar.fpscala.chapters.util._

class Option$Test extends FunSuite {

  val l: List[Double] = List(1.0, 2.0, 3.0, 4.0)

  test("The mean of a sequence") {
    assert(mean(l).get ~= 2.5)
  }

  test("The variance of a sequence") {
    assert(variance(l).get ~= 1.25)
  }
}
