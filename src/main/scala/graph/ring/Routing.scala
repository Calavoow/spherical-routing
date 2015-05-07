package graph.ring

import instrumentation.Metric.Router

import scala.annotation.tailrec
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import Units._
import graph.Util.TwoPower

object Routing extends Router[Node] {
		override def route(g: Graph[Node, UnDiEdge])(from: g.NodeT, to: g.NodeT) : g.Path = {
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
					val aNext = step(g)(as.head, from, i)
					val bNext = step(g)(as.head, to, i)
					recursion(aNext :: as, bNext :: bs, i+1)
				}
			}
		}

		// Build a path out of the node lists.
		val (as, bs) = recursion(List(from), List(to), 1)
		val pathBuilder = g.newPathBuilder(from)
		pathBuilder ++= as.reverse
		pathBuilder ++= bs
		pathBuilder.result()
	}


	private def step(g : Graph[Node, UnDiEdge])(head: g.NodeT, origin: g.NodeT, iteration: Int) : g.NodeT = {
		val twoI = (iteration-1).twoPowerOf
		val n = g.nodes.size
		// Explicitly get Int values, needed to perform Integer addition and not String concatenation.
		val headIndex : Int = head
		val originIndex : Int = origin
		if(head % twoI == 0) {
			head
		} else if (p(headIndex + twoI) > p(originIndex - twoI)) {
			val node = (headIndex + twoI) % n
			g.get(node)
		} else {
			val node = (headIndex - twoI) % n
			g.get(node)
		}
	}
}
