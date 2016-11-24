lazy val address_finder = (project in file(".")).
  aggregate(spark_jdbc_utils).
  settings(inThisBuild(List(
    organization := "uk.ac.cdrc.data",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.8",
    crossScalaVersions := Seq("2.10.8"))),
    packagedArtifacts := Map.empty,
    name := "address-finder",
    libraryDependencies  ++= Seq(
      "org.scalanlp" %% "breeze" % "0.11.2",
      "org.scalanlp" %% "breeze-natives" % "0.11.2",
      "org.scalanlp" %% "breeze-viz" % "0.11.2",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    )
  )


lazy val spark_jdbc_utils = (project in file("spark-jdbc-util")).
  settings(
    name := "spark-jdbc-utils",
    libraryDependencies  ++= Seq(
      "org.apache.spark" %% "spark-sql" % "2.0.0",
      "org.postgresql" % "postgresql" % "9.4.1212",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    )
  )
