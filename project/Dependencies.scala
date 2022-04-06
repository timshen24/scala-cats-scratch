import sbt._

object Dependencies {
  val cats = "org.typelevel" %% "cats-core" % "2.7.0"
  val kantanGeneric = "com.nrinaudo"      %% "kantan.csv"      % "0.6.1"
  val scalatest     = "org.scalatestplus" %% "scalacheck-1-14" % "3.1.2.0" % Test
  val kindProjector = "org.typelevel"     %% "kind-projector"  % "0.10.3" cross CrossVersion.binary // does not support Scala3
}
