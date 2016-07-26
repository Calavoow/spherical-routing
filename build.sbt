name := "Spherical Routing"

version := "0.5"

organization := "eu.calavoow"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
	"com.assembla.scala-incubator" %% "graph-core" % "1.11.0",
	"com.assembla.scala-incubator" %% "graph-dot" % "1.11.0",
	"org.scalatest" % "scalatest_2.11" % "2.2.6" % "test"
)

scalacOptions ++= Seq("-feature")

fork in run := true

connectInput in run := true

javaOptions += "-Xmx7168m"

javaOptions += "-Xms4096m"

test in assembly := {}
