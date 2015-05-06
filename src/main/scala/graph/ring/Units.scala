package graph.ring

import scala.annotation.tailrec
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
		val nrNodes = 4 * subdivisions.twoPowerOf
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
	def p(x: Int) : Int = {
		if(x == 0) Int.MaxValue
		else {
			val k = 31 - Integer.numberOfLeadingZeros(x)
			// The closest 2^k to x, rounded down.
			val powTwo = k.twoPowerOf
			gcd(x, powTwo)
		}
	}

	@tailrec
	def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
}
