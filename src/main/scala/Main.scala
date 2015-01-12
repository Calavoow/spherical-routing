import graph.SphereApproximation
import graph.Units.Node

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Main {
	def main(args: Array[String]) {
		val g = Graph[Node, UnDiEdge](
			List(1) ~ List(2),
			List(2) ~ List(3),
			List(3) ~ List(1),
			List(2) ~ List(4),
			List(4) ~ List(3),
			List(1) ~ List(5)
		)
		SphereApproximation.subdivide(g)
	}
}
