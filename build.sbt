name := "ChessEngine"

version := "0.1"

scalaVersion := "2.13.3"

unmanagedJars in Compile += file(Path.userHome + "/Your-Jar-Path/Full-Jar-Name.jar")

libraryDependencies ++= Seq(
  "net.liftweb" %% "lift-json" % "3.4.3",
  "org.apache.ivy" % "ivy" % "2.5.0",
  "com.dropbox.core" % "dropbox-core-sdk" % "3.1.5",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "org.scalanlp" %% "breeze" % "1.1",
  "org.scalanlp" %% "breeze-natives" % "1.1",
  "org.scalanlp" %% "breeze-viz" % "1.1",
  "com.lihaoyi" %% "pprint" % "0.5.6",
)