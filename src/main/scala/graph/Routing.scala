package graph

import graph.Units.{LabelEntry, Node}

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
				val paths = for (parent1 <- from.label.head;
				                 parent2 <- to.label.head)
					yield {
						val p1 = g0.nodes.find(_.id == parent1.id).get
						val p2 = g0.nodes.find(_.id == parent2.id).get
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
		val toParent = labelRoute(g)(child = from, parent = ancestorPath.nodes.head)
		val fromParent = labelRoute(g)(child = to, parent = ancestorPath.nodes.last)
		// Construct the complete path.
		path.++=(toParent.nodes).++=(ancestorPath.nodes).++=(fromParent.nodes).result()
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
		// Combine the labels, and extend with empty sets if one node is of a different layer.
		val zippedLabels = node1.label.zipAll(node2.label, Set.empty[LabelEntry], Set.empty[LabelEntry])
		val closestAncestor = zippedLabels.reverseMap {
			// Intersect each label_k with the other, to see if any ID occurs in both.
			case (l1, l2) => l1.map(_.id) intersect l2.map(_.id)
		} find {
			// Find the first common ancestor.
			_.nonEmpty
		} map { ids ⇒
			// Randomly decide betwmap(_.id).max.
			Random.shuffle(ids).head
		}

		// Convert the ID into a node.
		closestAncestor.map { id: Int =>
			// The ancestor must exist.
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
			val eligibleNeighbors = child.label.reverse.find { labelEntries ⇒
				// Get the closest set of eligible neighbors.
				// Eligible here means that it occurs in the label of the child node.
				(labelEntries.map(_.id) intersect neighborIds).nonEmpty
			} get // There must be an eligible neighbor.


			// Now convert that set of neighbours into a node.
			val eligibleIds = eligibleNeighbors.map(_.id)
			val bestNeighbor = parent.neighbors.find { neighbor ⇒
				eligibleIds.contains(neighbor.id)
			} get // Assume it exists.

			// Prepend the parent to the path. The new parent is the best neighbour.
			recursiveLabelRoute(g)(child, bestNeighbor, parent :: path)
		}
	}
}