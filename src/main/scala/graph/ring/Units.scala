package graph.ring

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

import graph.Util.TwoPower

import scala.language.postfixOps

object Units {
	type Node = Int

	def ring(subdivisions: Int) : Graph[Node, UnDiEdge]= {
		// Assume that the nr of nodes fits in an Int.
		// (Some collections dont support more than Int.MaxValue elements anyway.)
		val nrNodes = 4 * BigInt(2).pow(subdivisions).toInt
		val nodes = 0 until nrNodes

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
	def p(x: Int) : Int = {
		val qualifyingK = Iterator.from(0).takeWhile{ k ⇒
			// x (mod 2^k) = 0
			x % BigInt(2).pow(k).toInt == 0
		}
		qualifyingK.toSeq.last
	}
}
