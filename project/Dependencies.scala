import sbt._

object Version {
  val scala     = "2.11.5"
  val fpscala   = "0.0.1"
}

object Library {
  val junit     = "junit"         %  "junit"     % "4.12"
  val scalaTest = "org.scalatest" %% "scalatest" % "2.2.4"
}

object Dependencies {
  import Library._

  val fpscala = List(
    junit,
    scalaTest
  )
}