package graph

import graph.Units._

import scala.annotation.tailrec
import scala.language.higherKinds
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

object Util {
	implicit class RichGraph[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) {
		/**
		 * Calculate the diameter of a graph.
		 * @return The diameter of a graph.
		 */
		def diameter : Int = {
			val nodes = g.nodes
			(for (node ← nodes;
			                  toNode ← nodes) yield {
				val path = node.shortestPathTo(toNode)
				path.map(p ⇒ p.edges.size).getOrElse(Int.MaxValue)
			}).max
		}
	}

	/**
	 * Find the set of triangles, where each triangles is represented by a set of 3.
	 *
	 * The scala.collection.Set is required because no immutable.Set[A] is returned by the for.
	 * @param g The graph to look into
	 * @return The set of triangles
	 */
	def triangles(g: Graph[Node, UnDiEdge]) : scala.collection.Set[Set[g.NodeT]] = {
		for(node ← g.nodes;

            // The subgraph containing neighbors of node and their edges
		    subGraph = g.filter(g.having(_.diSuccessors.contains(node)));

		    // For every node in the subgraph, find all adjacent nodes and create a triangle.
			adjacentNode ← subGraph.nodes;
			triangleNode ← adjacentNode.diSuccessors
		) yield {
			// A triangle is represented by a Set of size 3.
			// This way triangles can easily be made distinct.
			Set[g.NodeT](node,g.get(adjacentNode),g.get(triangleNode))
		}
	}

	def trianglesAround(g: Graph[Node,UnDiEdge])(node: g.NodeT) : scala.collection.Set[Set[g.NodeT]] = {
		// The subgraph containing neighbors of node and their edges
		val subGraph = g.filter(g.having(_.diSuccessors.contains(node)))
		for(
			// For every node in the subgraph, find all adjacent nodes and create a triangle.
		    adjacentNode ← subGraph.nodes;
		    triangleNode ← adjacentNode.diSuccessors
		) yield {
			// A triangle is represented by a Set of size 3.
			// This way triangles can easily be made distinct.
			Set[g.NodeT](node,g.get(adjacentNode),g.get(triangleNode))
		}
	}

	@tailrec
	def factorial[T: Numeric](n: T, accum: T= 0): T = {
		import Numeric.Implicits._
		n match {
			case 0 => accum
			case _ => factorial(n - implicitly[Numeric[T]].fromInt(1), n * accum)
		}
	}
	def binomCoef[T : Fractional](n: T, k: T) = {
		import Fractional.Implicits._
		Integral

		factorial(n) / (factorial(k) * factorial(n-k))
	}
}
