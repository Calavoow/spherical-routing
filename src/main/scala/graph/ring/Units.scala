package graph.ring

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

import scala.language.postfixOps

object Units {
	type Node = BigInt

	def ring(subdivisions: Int) : Graph[Node, UnDiEdge]= {
		val nrNodes = 4 * BigInt(2).pow(subdivisions)
		val nodes = BigInt(0) until nrNodes

		/**
		 * Make a ring between the given nodes.
		 *
		 * @param nodes A sequence of nodes.
		 * @return The edges that form a ring.
		 */
		def sequentialEdges(nodes: Seq[Node]) : Seq[UnDiEdge[Node]] = {
			(nodes :+ nodes.head).sliding(2).map {
				case Seq(n1, n2) ⇒ n1 ~ n2
			} toSeq
		}
		// For each cycle order, create edges.
		val maxOrder = p(nrNodes - 1)
		val edges = (0 to maxOrder).map { order ⇒
			val orderNodes = nodes.filter(_ % BigInt(2).pow(order) == 0)
			sequentialEdges(orderNodes)
		} reduce(_ ++ _) // Flatten all edges to one big sequence.
		Graph.from[Node, UnDiEdge](edges = edges)
	}

	/**
	 * Calculate the cycle order given an index x.
	 * @param x The index of a node.
	 * @return The cycle order.
	 */
	def p(x: BigInt) : Int = {
		val qualifyingK = Iterator.from(0).takeWhile{ k ⇒
			x % BigInt(2).pow(k) == BigInt(0)
		}
		qualifyingK.toSeq.last
	}
}
