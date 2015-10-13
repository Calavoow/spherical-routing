package graph.ring

import graph.Util
import instrumentation.Metric.Router

import scala.annotation.tailrec
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import Units._
import graph.Util.TwoPower

object Routing extends Router[Node] {
	override def route(g: Ring)(from: g.NodeT, to: g.NodeT) : g.Path = {
		Random.setSeed(System.currentTimeMillis())
		/**
		 * Recursively fill the steps towards a common node.
		 *
		 * The returned lists have as head the common node, and as tail the originating node.
		 * @param as
		 * @param bs
		 * @param i
		 * @return
		 */
		@tailrec
		def recursion(as : List[g.NodeT], bs: List[g.NodeT], i: Int) : (List[g.NodeT], List[g.NodeT]) = {
			if(as.head.findOutgoingTo(bs.head).isDefined) {
				(as, bs)
			} else {
				val commonNeighbours = as.head.neighbors intersect bs.head.neighbors
				if(commonNeighbours.nonEmpty) {
					val randomNode = Random.shuffle(commonNeighbours.toSeq).head
					recursion(randomNode :: as, bs, i+1)
				} else {
					val aNext = step(g)(as.head, i)
					val bNext = step(g)(bs.head, i)
					recursion(aNext :: as, bNext :: bs, i+1)
				}
			}
		}

		// Build a path out of the node lists.
		val (as, bs) = recursion(List(from), List(to), 1)
		Util.joinPaths(g)(as.reverse, bs)
	}

	private def step(g : Graph[Node, UnDiEdge])(head: g.NodeT, iteration: Int) : g.NodeT = {
		val twoIMinusOne = (iteration-1).twoPowerOf
		val n = g.nodes.size
		// Explicitly get Int values, needed to perform Integer addition and not String concatenation.
		val headIndex : Int = head
		if(head % iteration.twoPowerOf == 0) {
			head
		} else if (p(headIndex + twoIMinusOne) > p(headIndex - twoIMinusOne)) {
			val node = (((headIndex + twoIMinusOne) % n) + n) % n
			g.get(node)
		} else {
			val node = (((headIndex - twoIMinusOne) % n) + n) % n
			g.get(node)
		}
	}
}
