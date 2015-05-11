package instrumentation

import graph.Util.Layered
import graph.sphere.Routing.SphereRouter
import graph.{Util, ring, sphere}

import scala.collection.mutable
import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.util.Random
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import Util.Layered

object Metric {
	trait Router[T] {
		def route(g: Graph[T,UnDiEdge])(node1: g.NodeT, node2: g.NodeT): g.Path
	}

	def countShortestPaths[T : Layered](g: Graph[T, UnDiEdge]): Map[Int, Int] = {
		val router = new Router[T] {
			override def route(g: Graph[T, UnDiEdge])(node1: g.NodeT, node2: g.NodeT): g.Path = {
				node1.shortestPathTo(node2).get
			}
		}
		countPaths(g)(router)
	}

	def countRoutingPaths(g: Graph[sphere.Units.Node, UnDiEdge], g0: Graph[sphere.Units.Node, UnDiEdge])
	                     (ancestorRouter: (g0.NodeT, g0.NodeT) => g0.Path): Map[Int, Int] = {
		import sphere.Units.Node
		countPaths(g)(SphereRouter(g0)(ancestorRouter))
	}

	def countPaths[T : Layered](g: Graph[T, UnDiEdge])(router: Router[T]) : Map[Int, Int] = {
		def pathToLayers(path: g.Path) : Map[Int, Int] = {
			// Cannot use edges, because they can occur twice in a path.
			val layers = path.nodes.toIterator.sliding(2).map {
				case Seq(node1, node2) =>
					// The layer of the edge.
					val layer1 = implicitly[Layered[T]].layer(node1)
					val layer2 = implicitly[Layered[T]].layer(node2)
					Math.max(layer1, layer2)
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

	def randomCollisionCount[T : Layered](g: Graph[T, UnDiEdge], concurrentPaths: Int, samples: Int)(router: Router[T]) : Seq[Option[Int]] = {
		val nodes = g.nodes.toVector
		def randomDifNodes(random: Random) : Stream[g.NodeT] = {
			Stream.continually(nodes(random.nextInt(nodes.size))).distinct
		}

		def pathsCollide(path1: g.Path, path2: g.Path) : Option[g.EdgeT] = {
			val edges1 = path1.edges.toSet
			path2.edges.find(edges1)
		}

		Random.setSeed(System.currentTimeMillis())
		// Parallellize sampling!
		(0 to samples).map { _ ⇒
			val threadLocalRandom = ThreadLocalRandom.current()
			// Draw `concurrentPaths`*2 distinct nodes, and calculate paths.
			val nodes = randomDifNodes(threadLocalRandom).take(2*concurrentPaths)
			val routes = nodes.grouped(2).map {
				case Seq(node1, node2) ⇒ router.route(g)(node1, node2)
			}
			// Check if any two paths collide.
			val checkCollision = routes.foldLeft[(Set[g.EdgeT], Option[g.EdgeT])]((Set.empty, None)) {
				case ((previousEdges, collision), path) ⇒
					val collidingEdge = collision.orElse(path.edges.find(previousEdges))
					(previousEdges ++ path.edges, collidingEdge)
			}
			val collidingEdge = checkCollision._2
			collidingEdge.map { e ⇒
				Layered.edgeLayer[T](e.toOuter).layer(e.toOuter)
			}
		}
		// Reserialize
//		sampled.seq
	}
}
