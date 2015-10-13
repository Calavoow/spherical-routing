package graph.sphere

import graph.Util
import instrumentation.Metric.Router

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.language.postfixOps
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.immutable.Graph
import Units._

object Routing extends Router[Node] {
	override def route(g: Sphere)(node1: g.NodeT, node2: g.NodeT): g.Path = {
		Random.setSeed(System.currentTimeMillis())
		pathRecursive(g)(node1, node2, List.empty, List.empty)
	}

	/**
	 * An implementation of route on the Sphere that is tail recursive.
	 */
	@tailrec
	def pathRecursive(g: Sphere)(alpha: g.NodeT, beta: g.NodeT, pathA: List[g.NodeT], pathB: List[g.NodeT]) : g.Path = {
		val localPath = localSearch(g)(alpha, beta)
		localPath match {
			case Some(pab) =>
				val joinedPaths = Util.joinPaths(g)((alpha :: pathA).reverse, pab.nodes, beta :: pathB)
				joinedPaths
			case None =>
				if(beta.layer > alpha.layer) {
					pathRecursive(g)(alpha, incrementPath(g)(beta), pathA, beta :: pathB)
				} else {
					pathRecursive(g)(incrementPath(g)(alpha), beta, alpha :: pathA, pathB)
				}
		}
	}

	/**
	 * If d(alpha, beta) > 6 then do a step to a parent.
	 *
	 * This function calculates the parent to do a step to.
	 */
	def incrementPath(g: Sphere)(v: g.NodeT) : g.NodeT = {
		def pGood(vertices: Set[g.NodeT]): Set[g.NodeT] = {
			val parents = vertices.flatMap{ alpha =>
				SphereNode.parents(g)(alpha).toSet
			}

			// Each parent must be on the lowest layer of all parents.
			parents.filter( _.layer <= parents.map(_.layer).min )
		}

		val bSet = pGood(Set(v))

		// calculate f(aSet), the potential parents.
		val potentialParents = if(bSet.head.layer == 0) {
			bSet
		} else {
			val bSetGood = pGood(bSet)
			bSet.filter { beta =>
				val betaP = SphereNode.parents(g)(beta).toSet
				// Check whether p(beta) intersect pGood(bSet) is nonEmpty
				(betaP intersect bSetGood).nonEmpty
			}
		}

		// Pick a random element
		Random.shuffle(potentialParents.toSeq).head
	}

	/**
	 * LocalSearch looks in the 6th order neighbourhood of alpha for beta,
	 * and gives a path to it if its in the neighbourhood.
	 */
	def localSearch(g: Sphere)(alpha : g.NodeT, beta: g.NodeT) : Option[g.Path] = {
		val pathNodes = dijkstra6(g)(Queue(List(alpha)), beta, Set.empty[g.NodeT])
		pathNodes.map { nodes =>
			Util.joinPaths(g)(nodes)
		}
	}

	/**
	 * Dijkstra's algorithm limited to 6 hops.
	 *
	 * We assume that the graph is connected.
	 *
	 * @param g The graph.
	 * @param q The queue of paths to visit.
	 * @param dest The destination node.
	 * @param visited A set of nodes that have already been visited.
	 * @return Some path of nodes, or None if there is no path of less than 6.
	 */
	@tailrec
	def dijkstra6(g: Sphere)(q: Queue[List[g.NodeT]], dest: g.NodeT, visited: Set[g.NodeT]) : Option[List[g.NodeT]] = q match {
		case path +: poppedQ =>
			if(path.head == dest) {
				Some(path.reverse)
			} else {
				// Paths include start node, so 6 hops is 7 nodes.
				// Do not add paths to the queue, that are more than 6 hops.
				if(path.size < 7) {
					val newPaths = path.head.neighbors.filterNot(visited).map(_ :: path)
					dijkstra6(g)(poppedQ.enqueue(newPaths), dest, visited + path.head)
				} else {
					dijkstra6(g)(poppedQ, dest, visited + path.head)
				}
			}
		case _ => None
	}
}

