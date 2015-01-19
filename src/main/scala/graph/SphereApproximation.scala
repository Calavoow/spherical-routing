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
		val tri = triangles(g)
	}

	def triangles(g: Graph[Node, UnDiEdge]) = {
		val r = for(node ← g.nodes) yield {
			// The subgraph containing neighbors of node and their edges
			val subGraph = g.filter(g.having(_.diSuccessors.contains(node)))

			// For every node in the subgraph, find all adjacentnodes and create a triangle.
			for(adjacentNode ← subGraph.nodes;
						triangleNode ← adjacentNode.diSuccessors) yield {
				// A triangle is represented by a Set of size 3.
				// This way triangles can easily be made distinct.
				Set(node,adjacentNode,triangleNode)
			}
		}
		r.reduce(_ ++ _)
	}
}
