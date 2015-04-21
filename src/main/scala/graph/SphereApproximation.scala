package graph

import graph.Units.{Label, Node}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import scala.language.postfixOps

object SphereApproximation {

	def approximateSphere(k: Int) = {
		val ico = Units.icosahedron
	}

	def repeatedSubdivision(g: Graph[Node, UnDiEdge]): Iterator[Graph[Node, UnDiEdge]] = {
		Iterator.iterate(g) { graph =>
			subdivide(graph)
		}
	}

	def subdivide(g: Graph[Node, UnDiEdge]) : Graph[Node, UnDiEdge] = {
		// Calculate the current max label ID, and iteration number.
		val currentMaxLabel = g.nodes.map(_.id).max
		val iteration = g.nodes.map(_.label.size).max - 1

		// Find the edges that will be subdivided.
		val subdivisionEdges = g.edges.filter { edge ⇒
			edge.nodes.exists { node ⇒
				node.layer == iteration
			}
		}
		// Calculate which labels the new nodes should get.
		val newLabels = edgeLabels(g)(subdivisionEdges, currentMaxLabel)

		val newEdges = subdivisionEdges.par.flatMap { edge ⇒
			val currentLabel = newLabels(edge)
			// Collect the two triangles that have at least two nodes in common with the edge.
			val relTri = relevantTriangles(g)(edge, iteration)

			// Find the labels of the edges between the triangle nodes
			val toLabels = (for(relevantTriangle ← relTri;
			                    nodes ← relevantTriangle.subsets(2)) yield {
				val n1 :: n2 :: _ = nodes.toList
				val edge = n1.findOutgoingTo(n2).get

				newLabels(edge)
			}).-(currentLabel) // Do not create an edge to the node itself.

			val parentLabels = edge.nodes.toOuterNodes.toSet[Label]
			val allLabels = toLabels ++ parentLabels

			// Make an edge to every label
			for(label ← allLabels) yield {
				currentLabel ~ label
			}
		} seq

		// Add the new edges and their nodes to the graph.
		g.++(newEdges)
	}

	def edgeLabels(g: Graph[Node, UnDiEdge])(edges: Iterable[g.EdgeT], currentMaxLabel: Int): Map[g.EdgeT, Label] = {
		// Add a unique label to each edge.
		(for ((edge, index) <- edges.zipWithIndex) yield {
			val p1 :: p2 :: _ = edge.nodes.toList
			edge → Label(p1, p2, currentMaxLabel + index + 1)
		}).toMap
	}

	def relevantTriangles(g: Graph[Node, UnDiEdge])(e: g.EdgeT, iteration: Int) : Set[Set[g.NodeT]] = {
		val node1 :: node2 :: _ = e.nodes.toList
		val sameNeighbors = node1.neighbors.filter { neighbor ⇒
			val neighborsBoth = node2.neighbors.contains(neighbor)
			// Do not use edges of a previous layer.
			val correctLayer =
				(node1.layer == iteration || neighbor.layer == iteration) &&
				(node2.layer == iteration || neighbor.layer == iteration)
			neighborsBoth && correctLayer
		}

//		assert(sameNeighbors.size == 2, s"Same neighbors unexpected size ${sameNeighbors.size}: $sameNeighbors\n node1: ${node1.parentalLabel} / ${node1.neighbors}\n node2: ${node2.parentalLabel} / ${node2.neighbors}\n $e")
		sameNeighbors.map { neighbor ⇒
			Set(node1,node2,neighbor)
		}
	}
}
