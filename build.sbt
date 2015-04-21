name := "Spherical Routing"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
	"com.assembla.scala-incubator" %% "graph-core" % "1.9.1",
	"com.assembla.scala-incubator" %% "graph-dot" % "1.10.0",
	"org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)

scalacOptions ++= Seq("-feature")
