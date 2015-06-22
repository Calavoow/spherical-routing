package graph.ring

import scala.annotation.tailrec
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

import graph.Util.{ID, Layered, TwoPower}

import scala.language.postfixOps

object Units {
	type Node = Int
	type Ring = Graph[Node, UnDiEdge]

	/**
	 * Make a ring of the given number of subdivisions.
	 *
	 * The total number of nodes will be 4*2^`subdivisions`^
	 * @param subdivisions The number of times to subdivide.
	 * @return The ring
	 */
	def ring(subdivisions: Int) : Ring = {
		// Assume that the nr of nodes fits in an Int.
		// (Some collections dont support more than Int.MaxValue elements anyway.)
		val nrNodes = 4 * subdivisions.twoPowerOf
		val nodes = 0 until nrNodes

		/**
		 * Make a ring between the given nodes.
		 *
		 * @param nodes A sequence of nodes.
		 * @return The edges that form a ring.
		 **/
		def sequentialEdges(nodes: Seq[Node]) : Seq[UnDiEdge[Node]] = {
			(nodes :+ nodes.head).sliding(2).map {
				case Seq(n1, n2) ⇒ n1 ~ n2
			} toSeq
		}
		// For each cycle order, create edges.
		val edges = (0 to (subdivisions+1)).map { order ⇒
			val orderNodes = nodes.filter { nodeID ⇒
				(nodeID % order.twoPowerOf) == 0
			}
			sequentialEdges(orderNodes)
		} reduce(_ ++ _) // Flatten all edges to one big sequence.
		Graph.from[Node, UnDiEdge](edges = edges)
	}

	/**
	 * Calculate the cycle order given an index x.
	 * @param x The index of a node.
	 * @return The cycle order.
	 */
	def p(x: Node) : Int = {
		Integer.numberOfTrailingZeros(x)
	}

	/**
	 * The cycle order of the edge between x and y.
	 * @param x
	 * @param y
	 * @return
	 */
	def q(x: Node, y: Node) : Int = p(x).min(p(y))

	@tailrec
	def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)


	implicit object LayeredNode extends Layered[Node] {
		override def layer(a: Node, nrLayers: Int): Int = {
			// Correct layer to "base" layer, as 0.
			if(p(a) == 32) {
				0
			} else {
				Math.max(nrLayers - p(a) - 1, 0)
			}
		}
	}

	implicit object IdNode extends ID[Node] {
		override def id(a: Node) : Int = a
	}
}
