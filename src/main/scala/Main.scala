import graph.SphereApproximation
import graph.Units.{Label, Node}

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Main {
	def main(args: Array[String]) {
		val g = Graph[Node, UnDiEdge](
			Label(0,1) ~ Label(0,2),
			Label(0,2) ~ Label(0,3),
			Label(0,3) ~ Label(0,1),
			Label(0,2) ~ Label(0,4),
			Label(0,4) ~ Label(0,3),
			Label(0,1) ~ Label(0,5)
		)
		val subdivided = SphereApproximation.subdivide(g)
		println(subdivided)
	}
}
