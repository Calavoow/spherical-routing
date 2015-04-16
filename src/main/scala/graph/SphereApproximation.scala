package graph

import graph.Units.{Label, Node}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

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
		val maxLabels = for(node ← g.nodes;
			labelSet ← node.parentalLabel) yield {
			labelSet.max
		}
		val currentMaxLabel = maxLabels.max
		val iteration = g.nodes.map(_.parentalLabel.size).max - 1
		// Calculate which labels the new nodes should get.
		val newLabels = edgeLabels(g)(currentMaxLabel)

		val newEdges = g.edges.toIterable.par.flatMap { edge ⇒
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
		}

		// Add the new edges and their nodes to the graph.
		g.++(newEdges.seq)
	}

	def edgeLabels(g: Graph[Node, UnDiEdge])(currentMaxLabel: Int): Map[g.EdgeT, Label] = {
		// Add a unique label to each edge.
		(for ((edge, index) <- g.edges.zipWithIndex) yield {
			val p1 :: p2 :: _ = edge.nodes.toList
			val parentsLabel = p1.parentalLabel.zipAll(p2.parentalLabel, Set.empty[Int], Set.empty[Int]).map {
				case (s1, s2) ⇒ s1 union s2
			}
			edge → Label(parentsLabel :+ Set(currentMaxLabel + index + 1))
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
