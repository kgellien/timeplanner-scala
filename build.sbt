name := "timeplanner"

version := "0.7.6-SNAPSHOT"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "org.scala-lang" % "scala-parser-combinators" % "2.11.0-M4"

libraryDependencies += "joda-time" % "joda-time" % "2.3"

libraryDependencies += "org.joda" % "joda-convert" % "1.6"

libraryDependencies += "org.specs2" % "specs2_2.11" % "2.3.12" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"
