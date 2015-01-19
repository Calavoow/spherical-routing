package graph

import scalax.collection.Graph
import scalax.collection.GraphPredef.EdgeLikeIn

object Util {
	implicit class RichGraph[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) {
		def diameter = {
			val nodes = g.nodes
			(for (node ← nodes;
			                  toNode ← nodes) yield {
				val path = node.shortestPathTo(toNode)
				path.map(p ⇒ p.edges.size).getOrElse(Int.MaxValue)
			}).max
		}
	}
}
