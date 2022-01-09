name := "aoc-2020-scala"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.13.1" % "test")
libraryDependencies  ++= Seq("org.scalanlp" %% "breeze" % "2.0.1-RC1")
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.2"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.5"
