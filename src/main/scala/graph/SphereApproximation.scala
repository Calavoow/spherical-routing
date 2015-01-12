package graph

import graph.Units.Node

import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object SphereApproximation {

	def approximateSphere(k: Int) = {
		val ico = Units.icosahedron

	}

	def subdivide(g: Graph[Node, UnDiEdge]) = {
		// Each set represents a triangle and because it is a set of sets, any duplicate sets are filtered out
		val n = g.nodes.head.neighbors.subsets(2)
		val tri = triangles(g)
	}

	def triangles(g: Graph[Node, UnDiEdge]) = {
		for(node ← g.nodes;
		    // All neighbour subsets of size 2
		    adjacentNeighbours ← node.neighbors.subsets(2)
		    // Check if the neighbours are adjacent
		    if adjacentNeighbours.forall(v ⇒ adjacentNeighbours.-(v).subsetOf(v.neighbors))
		) yield {
			// The 2 adjacent neighbours plus the current node form a triangle.
			adjacentNeighbours.+(node)
		}
	}
}
