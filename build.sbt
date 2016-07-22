name := "address-finder"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.6"

crossScalaVersions := Seq("2.11.8")

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12",
  "org.scalanlp" %% "breeze-natives" % "0.12",
  "org.scalanlp" %% "breeze-viz" % "0.12",
  "org.reactormonk" %% "counter" % "1.3.3",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

