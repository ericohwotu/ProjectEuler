
name := "project-euler"

version := "1.0"

scalaVersion := "2.12.1"

//wart remover error level declaration
wartremoverErrors ++= Warts.unsafe

// main scala test module import
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "org.scalautils" % "scalautils_2.11" % "2.1.5"

//junit test module import
libraryDependencies += "junit" % "junit" % "4.10" % "test"
