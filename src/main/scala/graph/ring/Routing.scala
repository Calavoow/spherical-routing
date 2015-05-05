package graph.ring

import scala.annotation.tailrec
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import Units._

object Routing {
	def route(g: Graph[Node,UnDiEdge])(from: g.NodeT, to: g.NodeT) = {
		Random.setSeed(System.currentTimeMillis())

	}

	def recursiveRoute(g: Graph[Node, UnDiEdge])(a: g.NodeT, b: g.NodeT) : g.Path = {
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
			if(as.head.hasSuccessor(bs.head)) {
				(as, bs)
			} else {
				val commonNeighbours = as.head.neighbors intersect bs.head.neighbors
				if(commonNeighbours.nonEmpty) {
					val randomNode = Random.shuffle(commonNeighbours.toSeq).head
					recursion(randomNode :: as, bs, i+1)
				} else {
					def step(head: g.NodeT, origin: g.NodeT) : g.NodeT = {
						val twoI = BigInt(2).pow(i)
						val n = g.nodes.size
						val headIndex : BigInt = head
						val originIndex : BigInt = origin
						if(head.mod(twoI) == BigInt(0)) {
							head
						} else if (p(headIndex + twoI) > p(originIndex - twoI)) {
							val node = (headIndex + twoI).mod(n)
							g.get(node)
						} else {
							val node = (headIndex - twoI).mod(n)
							g.get(node)
						}
					}
					val aNext = step(as.head, a)
					val bNext = step(as.head, b)
					recursion(aNext :: as, bNext :: bs, i+1)
				}
			}
		}

		// Build a path out of the node lists.
		val (as, bs) = recursion(List(a), List(b), 1)
		val pathBuilder = g.newPathBuilder(a)
		for(aEl ← as.reverse) {
			assert(pathBuilder.add(aEl))
		}
		for(bEl  ← bs) {
			assert(pathBuilder.add(bEl))
		}
		pathBuilder.result()
	}
}
