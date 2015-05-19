package instrumentation

import graph.Util.Layered
import graph.{Util, ring, sphere}

import scala.collection.mutable
import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.util.Random
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import Util.Layered

object Metric {
	trait Router[N] {
		def route(g: Graph[N, UnDiEdge], graphSize: Int)(node1: g.NodeT, node2: g.NodeT, nodeMap: IndexedSeq[g.NodeT]): g.Path
	}

	def countShortestPaths[N : Layered](g: Graph[N, UnDiEdge], nrLayers: Int): Map[Int, Int] = {
		countPaths(g, nrLayers)(Util.ShortestPathRouter())
	}

	def countRoutingPaths(g: Graph[sphere.Units.Node, UnDiEdge], g0: Graph[sphere.Units.Node, UnDiEdge], nrLayers: Int)
	                     (ancestorRouteMap: Map[(g0.NodeT, g0.NodeT),g0.Path]): Map[Int, Int] = {
		countPaths(g, nrLayers)(sphere.Routing.sphereRouter(g0)(ancestorRouteMap))
	}

	/**
	 * Count the number of paths that use a given layer.
	 *
	 * @param g
	 * @param router The routing function to use for routing on the given Graph.
	 * @tparam T
	 * @return A map of Layer of edge -> nr of paths that used this edge.
	 */
	def countPaths[T : Layered](g: Graph[T, UnDiEdge], nrLayers: Int)(router: Router[T]) : Map[Int, Int] = {
		def pathToLayers(path: g.Path) : Map[Int, Int] = {
			// Cannot use edges, because they can occur twice in a path.
			val layers = path.nodes.toIterator.sliding(2).map {
				case Seq(node1, node2) =>
					// The layer of the edge.
					val layer1 = implicitly[Layered[T]].layer(node1, nrLayers)
					val layer2 = implicitly[Layered[T]].layer(node2, nrLayers)
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

		// Cache graphSize because it's O(n)
		val graphSize = g.nodes.size

		// Split into smaller computation groups, and reduce them individually.
		// To prevent memory issues.
		val combinationGroups = g.nodes.toSeq.combinations(2).grouped(50000)
		combinationGroups.map { group ⇒
			// Map the group in parallel.
			group.par.map({
				case Seq(node1, node2) =>
					router.route(g, graphSize)(node1, node2)
			})
				// Transform the path to a map of layer nr and count.
				.map(pathToLayers)
				// Reduce the map of keys to save memory size.
				.reduce(sumMapByKey)
		} reduce(sumMapByKey)
	}

	def randomCollisionCount[T : Layered](g: Graph[T, UnDiEdge], nrLayers: Int, concurrentPaths: Int, samples: Int)(router: Router[T]) : Seq[Option[Int]] = {
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
		val sampled = (0 to samples).par.map { _ ⇒
			val threadLocalRandom = ThreadLocalRandom.current()
			// Draw `concurrentPaths`*2 distinct nodes, and calculate paths.
			val nodes = randomDifNodes(threadLocalRandom).take(2*concurrentPaths)
			val routes = nodes.grouped(2).map {
				case Seq(node1, node2) ⇒ router.route(g, nodes.size)(node1, node2)
			}
			// Check if any two paths collide.
			val checkCollision = routes.foldLeft[(Set[g.EdgeT], Option[g.EdgeT])]((Set.empty, None)) {
				case ((previousEdges, collision), path) ⇒
					val collidingEdge = collision.orElse(path.edges.find(previousEdges))
					(previousEdges ++ path.edges, collidingEdge)
			}
			val collidingEdge = checkCollision._2
			collidingEdge.map { e ⇒
				Layered.edgeLayer[T](e.toOuter).layer(e.toOuter, nrLayers)
			}
		}
		// Reserialize
		sampled.seq
	}
}
