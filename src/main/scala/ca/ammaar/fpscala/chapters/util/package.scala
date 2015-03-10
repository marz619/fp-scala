package ca.ammaar.fpscala.chapters

package object util {

  case class Precision(p: Double)

  implicit val p: Precision = Precision(1e-6)

  implicit class DoubleWithAlmostEquals(val d: Double) extends AnyVal {
    def ~=(o: Double)(implicit p: Precision) = (d - o).abs < p.p
  }
}
