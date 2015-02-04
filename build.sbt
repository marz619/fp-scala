organization := "ca.ammaar"

name := "fp-scala"

version := Version.fpscala

scalaVersion := Version.scala

resolvers ++= List(
  "Sonatype OSS Releases"  at "https://oss.sonatype.org/content/repositories/releases",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Dependencies.fpscala
