resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

lazy val address_finder = (project in file(".")).
  aggregate(spark_jdbc_util).
  settings(inThisBuild(List(
    organization := "uk.ac.cdrc.data",
    version := "0.2-SNAPSHOT",
    scalaVersion := "2.11.8",
    crossScalaVersions := Seq("2.10.8"))),
    name := "address-finder",
    libraryDependencies  ++= Seq(
      "org.scalanlp" %% "breeze" % "0.13",
      "org.scalanlp" %% "breeze-natives" % "0.13",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    )
  )


lazy val spark_jdbc_util = (project in file("spark-jdbc-util")).
  settings(
    name := "spark-jdbc-util",
    libraryDependencies  ++= Seq(
      "org.apache.spark" %% "spark-sql" % "2.1.0",
      "org.postgresql" % "postgresql" % "9.4.1212",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"
    )
  )
