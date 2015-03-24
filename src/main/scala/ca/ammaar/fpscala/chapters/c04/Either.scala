package ca.ammaar.fpscala.chapters.c04

import scala.{Either => _} // hides the standard library implementation

sealed trait Either[+E, +A] {

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: Seq[Double]): Either[String, Double] = {
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)
  }
}