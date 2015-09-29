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
	override def route(g: Sphere, graphSize: Int)(node1: g.NodeT, node2: g.NodeT, nodeMap: IndexedSeq[g.NodeT]): g.Path = {
		pathRecursive(g)(node1, node2, List.empty, List.empty)
	}

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

	def incrementPath(g: Sphere)(v: g.NodeT) : g.NodeT = {
		def pGood(vertices: Set[g.NodeT]): Set[g.NodeT] = {
			val parents = vertices.flatMap{ alpha =>
				SphereNode.parents(g)(alpha).toSet
			}

			// Each parent must be on the lowest layer of all parents.
			parents.filter( _.layer <= parents.map(_.layer).min )
		}

		val bSet = pGood(Set(v))

		// calculate f(aSet)
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

	def localSearch(g: Sphere)(alpha : g.NodeT, beta: g.NodeT) : Option[g.Path] = {
		val pathNodes = dijkstra6(g)(Queue(List(alpha)), beta, Set.empty[g.NodeT])
		pathNodes.map { nodes =>
			val builder = g.newPathBuilder(alpha)
			builder ++= nodes
			builder.result()
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
	def dijkstra6(g: Sphere)(q: Queue[List[g.NodeT]], dest: g.NodeT, visited: Set[g.NodeT]) : Option[List[g.NodeT]] = {
		val (path, poppedQ) = q.dequeue
		if (path.size > 7) {
			// Paths include start node, so 6 hops is 7 nodes.
			// Stop as soon as we start processing paths of more than 6 hops.
			None
		} else if(path.head == dest) {
			Some(path.reverse)
		} else {
			path.head.diSuccessors
			val newPaths = path.head.neighbors.filterNot(visited).map(_ :: path)
			val newQ = poppedQ.enqueue(newPaths)
			dijkstra6(g)(newQ, dest, visited + path.head)
		}
	}

}

