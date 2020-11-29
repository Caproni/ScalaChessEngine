name := "ChessEngine"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest" % "3.2.3" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "org.scalanlp" %% "breeze" % "1.1",
  "org.scalanlp" %% "breeze-natives" % "1.1",
  "org.scalanlp" %% "breeze-viz" % "1.1",
  "com.lihaoyi" %% "pprint" % "0.5.6",
)