import Dependencies._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.6"

lazy val baseSettings: Seq[Setting[_]] = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:postfixOps",
    "-unchecked",
    "-Ywarn-value-discard",
    //    "-Wconf:cat=other-match-analysis:error", // uncomment to transform non-exhaustive warnings into errors
    //    "-Wconf:cat=unchecked:error",            // uncomment to transform type erasure warnings into errors
  ),
  addCompilerPlugin(kindProjector),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.7.0",
    "org.typelevel" %% "cats-laws" % "2.7.0",
    "org.typelevel" %% "discipline-core" % "1.4.0" % Test,
    "org.typelevel" %% "discipline-scalatest" % "2.1.5" % Test,
    "org.scalatest" %% "scalatest" % "3.2.11" % Test
  )
)

lazy val root = (project in file("."))
  .settings(
    name := "scala-cats-scratch"
  )

lazy val fun_cats = project
  .settings(baseSettings: _*)

lazy val scala_with_cats = project
  .settings(baseSettings: _*)
