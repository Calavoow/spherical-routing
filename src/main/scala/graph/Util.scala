package graph

import graph.sphere.Units
import Units._
import instrumentation.Metric.Router

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.{higherKinds,implicitConversions}
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
		/**
		 * The layer should be the number of swaps necessary to create this edge.
		 *
		 * @param a
		 * @param nrLayers
		 * @return
		 */
		def layer(a: T, nrLayers: Int): Int
	}
	object Layered{
		implicit def edgeLayer[T: Layered](edge : UnDiEdge[T]) : Layered[UnDiEdge[T]] = new Layered[UnDiEdge[T]] {
			override def layer(e: UnDiEdge[T], nrLayers: Int) : Int = {
				val Seq(node1, node2) = edge.nodeSeq
				val layer1 = implicitly[Layered[T]].layer(node1, nrLayers)
				val layer2 = implicitly[Layered[T]].layer(node2, nrLayers)
				Math.min(layer1, layer2)
			}
		}
	}

	trait ID[T] {
		def id(a: T): Int

	}

	def toIdSeq[T: ID](ids: Traversable[T]) : IndexedSeq[T] = {
		val nrIds = ids.map { element ⇒
			implicitly[ID[T]].id(element)
		}.max
		val idMap = new ArrayBuffer[T](nrIds)
		ids.foreach { element ⇒
			val id = implicitly[ID[T]].id(element)
			idMap(id) = element
		}
		idMap
	}


	/**
	 * A router that uses the shortestPathTo method of the Graph library.
	 *
	 * An object implementation is not possible (i.e. with Router[Nothing]), because in Graph[N,E], N is invariant.
	 * @tparam N The node type
	 */
	case class ShortestPathRouter[N]() extends Router[N] {
		override def route(g: Graph[N, UnDiEdge], graphSize: Int)(node1: g.NodeT, node2: g.NodeT, nodeMap: IndexedSeq[g.NodeT]): g.Path = {
			node1.shortestPathTo(node2).get
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

	def allShortestPaths[N](g: Graph[N,UnDiEdge]) : Map[(g.NodeT, g.NodeT), g.Path] = {
		g.nodes.toSeq.combinations(2).map {
			case Seq(node1, node2) ⇒ (node1, node2) → node1.shortestPathTo(node2).get
		}.toMap
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
