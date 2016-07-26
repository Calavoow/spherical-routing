package instrumentation

import graph.Util.{ID, Layered}
import graph.{Util, ring, sphere}

import scala.collection.mutable
import scala.concurrent.forkjoin.ThreadLocalRandom
import scala.util.Random
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Metric {
	trait Router[N] {
		/**
		 * Route on the graph {{g}} from {{node1}} to {{node2}}.
		 *
		 * We assume that the graph is connected.
		 *
		 * @param g The graph to route on.
		 * @param node1 The start node.
		 * @param node2 The end node.
		 * @return A path on {{g}} from start to end node.
		 */
		def route(g: Graph[N, UnDiEdge])(node1: g.NodeT, node2: g.NodeT): g.Path
	}

	def countShortestPaths[N : Layered : ID](g: Graph[N, UnDiEdge], nrLayers: Int): Map[Int, Int] = {
		countPaths(g, nrLayers)(Util.ShortestPathRouter())
	}

	def countRoutingPaths(g: Graph[sphere.Units.Node, UnDiEdge], g0: Graph[sphere.Units.Node, UnDiEdge], nrLayers: Int)
	                     (ancestorRouteMap: Map[(Int, Int),g0.Path]): Map[Int, Int] = {
		countPaths(g, nrLayers)(sphere.Routing)
	}

	/**
	 * Count the number of paths that use a given layer.
	 *
	 * @param g The graph to count paths on.
	 * @param router The routing function to use for routing on the given Graph.
	 * @tparam T The node type.
	 * @return A map of Layer of edge -> nr of paths that used this edge.
	 */
	def countPaths[T : Layered : ID](g: Graph[T, UnDiEdge], nrLayers: Int)(router: Router[T]) : Map[Int, Int] = {
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
//		val graphSize = g.nodes.size
		// Create a nodeMap because looking up the ID is also O(n)
//		val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[T]].id(node))

		// Split into smaller computation groups, and reduce them individually.
		// To prevent memory issues.
		val combinationGroups = g.nodes.toSeq.combinations(2).grouped(50000)
		combinationGroups.map { group ⇒
			// Map the group in parallel.
			group.par.map{
				case Seq(node1, node2) =>
					router.route(g)(node1, node2)
			}
			// Transform the path to a map of layer nr and count.
			.map(pathToLayers _)
			// Reduce the map of keys to save memory size.
			.reduce(sumMapByKey)
		}.reduce(sumMapByKey _)
	}

	/**
	 * Count the number of collisions between a given number of concurrent paths.
	 *
	 * Paths are randomly sampled from all nodes. Any two paths will have distinct endpoints.
	 * @param g The graph to sample from.
	 * @param nrLayers The number of layers that graph has.
	 * @param concurrentPaths The number of concurrent paths to sample.
	 * @param samples The number of random samples to take.
	 * @param router The Router to use to find a path between two endpoints.
	 * @tparam T The node type
	 * @return A sequence of the layer in which a collision occurred, if any.
	 */
	def randomCollisionCount[T : Layered : ID](g: Graph[T, UnDiEdge], nrLayers: Int, concurrentPaths: Int, samples: Int)(router: Router[T]) : Seq[Option[Int]] = {
		val nodes = g.nodes.toVector
		def randomDifNodes(random: Random) : Stream[g.NodeT] = {
			Stream.continually(nodes(random.nextInt(nodes.size))).distinct
		}

		// Create a nodeMap because looking up the ID is O(n)
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[T]].id(node))

		Random.setSeed(System.currentTimeMillis())
		// Parallellize sampling!
		val sampled = (0 to samples).par.map { _ ⇒
			val threadLocalRandom = ThreadLocalRandom.current()
			// Draw `concurrentPaths`*2 distinct nodes, and calculate paths.
			val nodes = randomDifNodes(threadLocalRandom).take(2*concurrentPaths)
			val nodeSeq = nodes.toIndexedSeq
			val routes = nodeSeq.grouped(2).map {
				case Seq(node1, node2) ⇒ router.route(g)(node1, node2)
			}
			// Check if any two paths collide.
			val collidingEdge = collisionEdge(g)(routes)
			collidingEdge.map { e ⇒
				Layered.edgeLayer[T](e.toOuter).layer(e.toOuter, nrLayers)
			}
		}
		// Reserialize
		sampled.seq
	}

	/**
	 * Find the edge in a set of paths where a collision occurred, if any.
	 *
	 * @param g The graph of the edge.
	 * @param paths A TraversableOnce of the paths between which a collision must be found.
	 * @tparam T The node type.
	 * @return The edge where a collision occurred.
	 */
	def collisionEdge[T](g: Graph[T, UnDiEdge])(paths: TraversableOnce[g.Path]) : Option[g.EdgeT] = {
		paths.foldLeft[(Set[g.EdgeT], Option[g.EdgeT])](Set.empty, None) {
			case ((previousEdges, collision), path) ⇒
				val collidingEdge = collision.orElse(path.edges.find(previousEdges))
				(previousEdges ++ path.edges, collidingEdge)
		}._2
	}
}
