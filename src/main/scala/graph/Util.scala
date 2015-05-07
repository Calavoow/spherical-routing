package graph

import graph.sphere.Units
import Units._

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

	implicit class TwoPower(num: Int) {
		/**
		 * Calculate two to the power of this integer.
		 *
		 * I.e. 2^num
		 * @return 2^num
		 */
		def twoPowerOf: Int = {
			1 << num
		}
	}

	trait Layered[T] {
		def layer(a: T): Int
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

	def factorial[T: Numeric](n: T) = recursiveFactorial(n, implicitly[Numeric[T]].fromInt(1))
	@tailrec
	private def recursiveFactorial[T: Numeric](n: T, accum: T): T = {
		import Numeric.Implicits._
		n match {
			case 0 => accum
			case _ => recursiveFactorial(n - implicitly[Numeric[T]].fromInt(1), n * accum)
		}
	}
	def binomCoef[T: Integral](n: T, k: T): T = {
		import Integral.Implicits._
		factorial(n) / (factorial(k) * factorial(n-k))
	}
}
