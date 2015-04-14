package graph

import graph.Units.{Label, Node}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object SphereApproximation {

	def approximateSphere(k: Int) = {
		val ico = Units.icosahedron
	}

	def subdivide(g: Graph[Node, UnDiEdge]) = {
		// Each set represents a triangle and because it is a set of sets, any duplicate sets are filtered out
		val tri = Util.triangles(g)

		val maxLabels = for(node ← g.nodes;
			labelSet ← node.parentalLabel) yield {
			labelSet.max
		}
		val currentMaxLabel = maxLabels.max
		// Add a unique label to each edge.
		val edgeLabels = (for((edge, index) <- g.edges.zipWithIndex) yield {
			val parents = edge.nodes.toIterator
			val p1 = parents.next
			val p2 = parents.next
			val parentsLabel = p1.parentalLabel.zipAll(p2.parentalLabel, Set.empty[Int], Set.empty[Int]).map {
				case (s1, s2) ⇒ s1 union s2
			}
			println(index)
			edge.toOuter → Label(parentsLabel :+ Set(currentMaxLabel + index + 1))
		}).toMap

		val newEdges = g.edges.flatMap { edge ⇒
			val currentLabel = edgeLabels(edge.toOuter)
			// Collect the two triangles that have at least two nodes in common with the edge.
			val relevantTriangles = tri.filter { triangle ⇒ edge.nodes.toSet.subsetOf(triangle)}

			// Find the labels of the edges between the triangle nodes
			val toLabels = relevantTriangles.flatMap { relevantTriangle ⇒
				// The subgraph spanning the triangle without the current edge
				val relevantGraph = g.filter(g.having(node = relevantTriangle.contains(_))) - edge
//				println(s"Relevant graph $relevantGraph")
				relevantGraph.edges.map { rEdge ⇒ edgeLabels(rEdge.toOuter) }
			}
			val parentLabels = edge.nodes.toOuterNodes.toSet
			val allLabels = toLabels ++ parentLabels

			// Make an edge to every label
			for(label ← allLabels) yield {
				currentLabel ~ label
			}
		}

		val newGraph = Graph.from[Node, UnDiEdge](edges = newEdges)
		g.++(newGraph)
	}
}
