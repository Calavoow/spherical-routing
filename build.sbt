name := "Spherical Routing"

version := "0.5"

organization := "eu.calavoow"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
	"com.assembla.scala-incubator" %% "graph-core" % "1.9.2",
	"com.assembla.scala-incubator" %% "graph-dot" % "1.10.0",
	"org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)

scalacOptions ++= Seq("-feature")

fork in run := true

connectInput in run := true

javaOptions in run += "-Xmx7168m"

javaOptions in run += "-Xms4096m"

test in assembly := {}
