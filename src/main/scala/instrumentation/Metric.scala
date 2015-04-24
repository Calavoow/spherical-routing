package instrumentation

import graph.Routing

import graph.Units._
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph
import scala.collection.mutable

object Metric {
	trait Router {
		def route(g: Graph[Node,UnDiEdge])(node1: g.NodeT, node2: g.NodeT): g.Path
	}

	def countShortestPaths(g: Graph[Node, UnDiEdge]): Map[Int, Int] = {
		val router = new Router {
			override def route(g: Graph[Node, UnDiEdge])(node1: g.NodeT, node2: g.NodeT): g.Path = {
				node1.shortestPathTo(node2).get
			}
		}
		countPaths(g)(router)
	}

	def countRoutingPaths(g: Graph[Node, UnDiEdge], g0: Graph[Node, UnDiEdge]): Map[Int, Int] = {
		val router = new Router {
			override def route(g: Graph[Node, UnDiEdge])(node1: g.NodeT, node2: g.NodeT): g.Path = {
				Routing.route(g, g0)(node1, node2)
			}
		}
		countPaths(g)(router)
	}

	def countPaths(g: Graph[Node, UnDiEdge])(router: Router) : Map[Int, Int] = {
		def pathToLayers(path: g.Path) : Map[Int, Int] = {
			val layers = path.nodes.toIterator.sliding(2).map {
				case Seq(node1, node2) =>
					// The layer of the edge.
					Math.max(node1.layer, node2.layer)
			}
			// Transform into Map(Layer -> Number of edges in that layer)
			layers.toSeq.groupBy(identity).mapValues(_.size)
		}

		def sumMapByKey(m: Map[Int, Int], n: Map[Int, Int]) : Map[Int, Int] = {
			m ++ n.map {
				case (k, v) ⇒ k → (v + m.getOrElse(k, 0))
			}
		}

		// Split into smaller computation groups, and reduce them individually.
		// To prevent memory issues.
		val combinationGroups = g.nodes.toSeq.combinations(2).grouped(50000)
		combinationGroups.map { group ⇒
			// Map the group in parallel.
			group.par.map({
				case Seq(node1, node2) =>
					router.route(g)(node1, node2)
			})
				// Transform the path to a map of layer nr and count.
				.map(pathToLayers)
				// Reduce the map of keys to save memory size.
				.reduce(sumMapByKey)
		} reduce(sumMapByKey)
	}


	def randomCollisionCount(g: Graph[Node, UnDiEdge], g0: Graph[Node, UnDiEdge], samples: Int) : Int = {
		val nodes = g.nodes.toVector
		val nodeProducer = Iterator.continually(nodes(Random.nextInt(nodes.size)))
		def randomDifNodes(count: Int) : Set[g.NodeT] = {
			val set = mutable.Set[g.NodeT]()
			nodeProducer.map { node ⇒
				set += node
			} takeWhile(_ ⇒ set.size < count) foreach { _ ⇒ } // force evaluation.
			set.toSet
		}

		def pathsCollide(path1: g.Path, path2: g.Path) : Boolean = {
			val edges1 = path1.edges.toSet
			path2.edges.exists(edges1)
		}

		Random.setSeed(System.currentTimeMillis())
		(0 to samples).par.count { _ ⇒
			val Seq(node1, node2, node3, node4) = randomDifNodes(4).toSeq
			val route1 = Routing.route(g, g0)(node1, node2)
			val route2 = Routing.route(g, g0)(node3, node4)
			pathsCollide(route1, route2)
		}
	}
}
