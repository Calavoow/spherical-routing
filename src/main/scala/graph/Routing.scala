package graph

import graph.Units.Node

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random
import scalax.collection.GraphEdge._
import scalax.collection.immutable.Graph

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
	def route(g: Graph[Node, UnDiEdge], g0: Graph[Node, UnDiEdge])(from: g.NodeT, to: g.NodeT): g.Path = {
		val path = g.newPathBuilder(from)
		path.add(from)
		// Step 1 Check if they are adjacent
		if( path.add(to) ) return path.result()

		// Try to find a common ancestor.
		val commonAncestor = closestAncestor(g)(from, to)
		// If there is no common ancestor, we need to route on the lowest layer. Otherwise we are already done.
		val ancestorPath: g.Path = commonAncestor match {
			case None =>
				// No common ancestor.
				// For efficiency, use the graph of only layer 0.
				val paths = for (parentID1 <- from.label.head if g0.nodes.exists(_.id == parentID1);
				                 parentID2 <- to.label.head if g0.nodes.exists(_.id == parentID2))
					yield {
						val p1 = g0.nodes.find(_.id == parentID1).get
						val p2 = g0.nodes.find(_.id == parentID2).get
						p1.shortestPathTo(p2).get // The graph is connected, so there is always a path.
					}
				val lowestPath = paths.minBy(_.size)

				// Convert lowestGraph.Path to our larger graph g.Path.
				val gNodes = lowestPath.nodes.map { lowNode =>
					g.get(lowNode)
				}
				val builder = g.newPathBuilder(gNodes.head)
				builder.++=(gNodes.tail).result()

			case Some(ancestor) =>
				// Otherwise construct an empty path.
				g.newPathBuilder(ancestor).result()
		}

		// Find the path to and from the parent.
		val fromToParent = labelRoute(g)(child = from, parent = ancestorPath.nodes.head)
		val toToParent = labelRoute(g)(child = to, parent = ancestorPath.nodes.last)
		// Construct the complete path.
		path.++=(fromToParent.nodes).++=(ancestorPath.nodes).++=(toToParent.nodes.toSeq.reverse).result()
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
		val firstNonEmptyIntersection = node1.label.view.reverseMap { entries1 =>
			node2.label.view.reverseMap(_ intersect entries1).find(_.nonEmpty)
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
			val builder = g.newPathBuilder(child)
			builder ++= path
			builder.result()
		} else {
			// Looking from the parent, see which neighbour comes closer to the child node.
			// Use the id, because the label entry may differ.
			val neighborIds = parent.neighbors.map(_.id)
			// Get the closest set of eligible neighbors.
			// Eligible here means that it occurs in the label of the child node.
			val eligibleNeighbors = child.label.reverse.find { labelEntries ⇒
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
}