organization := "com.github.tango238"

name := "fpinscala"

scalaVersion := "2.11.7"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.8.4" % "test",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

