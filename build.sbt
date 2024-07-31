import sbt.Keys.scalacOptions

val scala2Version = "2.12.4"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

mainClass in Compile := Some("retcalc.SimulatePlanApp")

lazy val root = (project in file("."))
  .settings(
    name := "retirement_calculator",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala2Version,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
      "org.typelevel" %% "cats-core" % "1.0.1"
    ),

    scalacOptions += "-Ypartial-unification"
  )
