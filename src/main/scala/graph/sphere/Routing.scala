package graph.sphere

import instrumentation.Metric.Router

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.immutable.Graph
import Units._

object Routing {

	/**
	 * Use the routing algorithm to find a route `from` to the vertex `to`.
	 *
	 * @param g The graph to route on.
	 * @param g0 The base graph (for efficiency). You may also use g0=g, but it is less efficient.
	 * @param from The node to route from.
	 * @param to The node to route towards.
	 * @return The route.
	 */
	def route(g: Graph[Node, UnDiEdge], g0: Graph[Node, UnDiEdge])
	         (from: g.NodeT, to: g.NodeT, ancestorRouteMap: Map[(g0.NodeT, g0.NodeT), g0.Path]): g.Path = {
		val path = g.newPathBuilder(from)(sizeHint = 64)
		path.add(from)
		// Step 1 Check if they are adjacent
		if( path.add(to) ) return path.result()

		// Try to find a common ancestor.
		val commonAncestor = closestAncestor(g)(from, to)
		// If there is no common ancestor, we need to route on the lowest layer. Otherwise we are already done.
		val ancestPath: g.Path = commonAncestor match {
			case None =>
				// No common ancestor.
				val node1Ancestors = from.label.last
				val node2Ancestors = to.label.last
				def g0Ancestors(ids: Set[Int]) = {
					g0.nodes.filter { node ⇒
						ids.contains(node.id)
					}
				}
				val g0Ancestors1 = g0Ancestors(node1Ancestors)
				val g0Ancestors2 = g0Ancestors(node2Ancestors)
				assert(g0Ancestors1.size > 0, s"Label was: ${from.label}, ancestors: $g0Ancestors1")
				assert(g0Ancestors2.size > 0, s"Label was: ${to.label}, ancestors: $g0Ancestors2")

				// Get the minimum path
				val paths = for(ancestor1 ← g0Ancestors1;
				                ancestor2 ← g0Ancestors2) yield {
					ancestorRouteMap.get((ancestor1, ancestor2)).map(_.nodes.toSeq)
						// It could also be store in reverse in the Map.
						.orElse(ancestorRouteMap.get((ancestor2, ancestor1)).map(_.nodes.toSeq.reverse))
						.get
				}
				val g0Path = paths.minBy { path ⇒
					path.size
				}

				// Convert g0.Path to our larger graph g.Path.
				val gNodes = g0Path.map { g0Node =>
					g.get(g0Node)
				}
				// Path is at most of length 3.
				val builder = g.newPathBuilder(gNodes.head)(sizeHint = 3)
				builder.++=(gNodes.tail).result()
			case Some(ancestor) =>
				// Otherwise construct an empty path.
				g.newPathBuilder(ancestor)(sizeHint = 1).result()
		}

		// Find the path to and from the parent.
		val fromToParent = labelRoute(g)(child = from, parent = ancestPath.nodes.head)
		val toToParent = labelRoute(g)(child = to, parent = ancestPath.nodes.last)
		// Construct the complete path.
		path.++=(fromToParent.nodes).++=(ancestPath.nodes).++=(toToParent.nodes.toSeq.reverse).result()
	}


	/**
	 * Find the closest common ancestor of two given nodes.
	 *
	 * This is the first ancestor of the nodes, which occurs in both their labels.
	 *
	 * @param g The graph which contains the nodes.
	 * @param node1 The first node.
	 * @param node2 The second node.
	 * @return The common ancestor. Could be None if there is no common ancestor in the label.
	 *         Then routing must occur on the lowest layer.
	 */
	def closestAncestor(g: Graph[Node, UnDiEdge])(node1: g.NodeT, node2: g.NodeT): Option[g.NodeT] = {
		val firstNonEmptyIntersection = node1.label.view.map { entries1 =>
			node2.label.view.map(_ intersect entries1).find(_.nonEmpty)
		} find (_.isDefined) flatten

		firstNonEmptyIntersection.map { intersection =>
			// Pick a random ancestor, if there are two equidistant.
			val id = Random.shuffle(intersection.toSeq).head
			// The ancestor must exist
			g.nodes.find(_.id == id).get
		}
	}

	/**
	 * Use the label to route from a parent node to a child node.
	 *
	 * This is more efficient than simply using Dijkstra.
	 * Note: It is assumed that `parent` is actually a parent of `child`. Otherwise an exception will be thrown.
	 *
	 * @param g The graph of the child and parent.
	 * @param child The child node.
	 * @param parent The parent node.
	 * @return A path from child to parent.
	 */
	def labelRoute(g: Graph[Node, UnDiEdge])(child: g.NodeT, parent: g.NodeT): g.Path = recursiveLabelRoute(g)(child, parent, Nil)

	@tailrec
	private def recursiveLabelRoute(g: Graph[Node, UnDiEdge])(child: g.NodeT, parent: g.NodeT, path: List[g.NodeT]): g.Path = {
		if( child == parent ) {
			// The base case, where the child node has been reached.
			val builder = g.newPathBuilder(child)(sizeHint = 32) // For 32 layers this size will be reached.
			builder ++= path
			builder.result()
		} else {
			// Looking from the parent, see which neighbour comes closer to the child node.
			// Use the id, because the label entry may differ.
			val neighborIds = parent.neighbors.map(_.id)
			// Get the closest set of eligible neighbors.
			// Eligible here means that it occurs in the label of the child node.
			val eligibleNeighbors = child.label.find { labelEntries ⇒
				(labelEntries intersect neighborIds).nonEmpty
			} get // There must be an eligible neighbor.


			// Now convert that set of neighbours into a node.
			val bestNeighbor = parent.neighbors.find { neighbor ⇒
				eligibleNeighbors.contains(neighbor.id)
			} get // It must exist, if there is an eligible neighbour.

			// Prepend the parent to the path. The new parent is the best neighbour.
			recursiveLabelRoute(g)(child, bestNeighbor, parent :: path)
		}
	}

	def sphereRouter(g0: Sphere)(ancestorMap: Map[(g0.NodeT, g0.NodeT), g0.Path]): Router[Node] = {
		new Router[Node] {
			override def route(g: Graph[Node, UnDiEdge])(node1: g.NodeT, node2: g.NodeT): g.Path = {
				Routing.route(g = g, g0 = g0)(node1, node2, ancestorMap)
			}
		}
	}
}

