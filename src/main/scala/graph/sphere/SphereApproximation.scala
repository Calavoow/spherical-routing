package graph.sphere

import scala.collection.mutable
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import Units._

object SphereApproximation {

	/**
	 * Approximate the sphere a given number of subdivision iterations.
	 * @param k The number of times to subdivide.
	 * @return The subdivide graph.
	 */
	def approximateSphere(k: Int) : Graph[Node, UnDiEdge] = {
		val ico = Units.icosahedron
		repeatedSubdivision(ico).drop(k-1).next()
	}

	/**
	 * An iterator through the subdivisions.
	 * @param g The initial graph to start with.
	 * @return The iterator.
	 */
	def repeatedSubdivision(g: Graph[Node, UnDiEdge]): Iterator[Graph[Node, UnDiEdge]] = {
		Iterator.iterate(g) { graph =>
			subdivide(graph)
		}
	}

	/**
	 * Subdivide the given graph once.
	 * @param g
	 * @return
	 */
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
		}.seq

		// Add the new edges and their nodes to the graph.
		g.++(newEdges)
	}

	/**
	 * Calculate the labels on the given edges.
	 * @param g
	 * @param edges The edges for which to calculate a label.
	 * @param currentMaxID The current maximum id.
	 * @return A mapping from edge to label.
	 */
	def edgeLabels(g: Graph[Node, UnDiEdge])(edges: Iterable[g.EdgeT], currentMaxID: Int): Map[g.EdgeT, Label] = {
		// Add a unique label to each edge.
		(for ((edge, index) <- edges.zipWithIndex) yield {
			val p1 :: p2 :: _ = edge.nodes.toList
			edge → Label(p1, p2, currentMaxID + index + 1)
		}).toMap
	}

	/**
	 * Given an edge, calculate which nodes the child of this edge will be connected to.
	 *
	 * The generated node needs to be connected to other children which are on the triangle with the generate node.
	 * Calculate the two triangles of these children.
	 * @param g
	 * @param e The edge which generates the node, for which to find triangles.
	 * @param iteration The current iteration number (k). Used to find only triangle involving only edges in E_k.
	 ang* @return
	 */
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

		sameNeighbors.map { neighbor ⇒
			Set(node1,node2,neighbor)
		}
	}
}
