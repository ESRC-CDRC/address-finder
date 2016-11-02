name := "address-finder"

organization := "uk.ac.cdrc.data"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.10.8")

libraryDependencies  ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
)

