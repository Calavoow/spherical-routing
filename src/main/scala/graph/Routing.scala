package graph

import graph.Units.{LabelEntry, Label, Node}

import scala.annotation.tailrec
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import util.Random
import scala.language.postfixOps

object Routing {
	/**
	 * Use the routing algorithm to find a route `from` to the vertex `to`.
	 *
	 * @param g The graph to route on.
	 * @param g0 The base graph (for efficiency).
	 * @param from The node to route from.
	 * @param to The node to route towards.
	 * @return The route.
	 */
	def route(g: Graph[Node, UnDiEdge], g0: Graph[Node, UnDiEdge])(from: g.NodeT, to: g.NodeT): g.Path = {
		val path = g.newPathBuilder(from)
		path.add(from)
		// Step 1 Check if they are adjacent
		if( path.add(to) ) return path.result()

		// First flatten the triangles into sets of Nodes, then intersect them to find a common node.
		val commonAncestor = closestAncestor(g)(from, to)
		val ancestorPath: g.Path = commonAncestor match {
			case None =>
				// For efficiency, reduce the graph to only layer 0.
				val paths = for ( parent1 <- from.label.head;
				                  parent2 <- to.label.head )
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

		val toParent = labelRoute(g)(child = from, parent = ancestorPath.nodes.head)
		val fromParent = labelRoute(g)(child = to, parent = ancestorPath.nodes.last)
		path.++=(toParent.nodes).++=(ancestorPath.nodes).++=(fromParent.nodes).result()
	}

	def closestAncestor(g: Graph[Node, UnDiEdge])(from: g.NodeT, to: g.NodeT): Option[g.NodeT] = {
		val zippedLabels = from.label.zipAll(to.label, Set.empty[LabelEntry], Set.empty[LabelEntry])
		val closestAncestor = zippedLabels.reverseMap {
			case (l1, l2) => l1.map(_.id) intersect l2.map(_.id)
		} find {
			// Find the first common ancestor.
			_.nonEmpty
		} map { ids ⇒
			//			if(ids.size > 1) println(ids)
			//			Random.shuffle(ids).head // Randomly decide betwmap(_.id).max.
			ids.max // Make choice deterministic for debugging.
		}

		// Convert the ID into a node.
		closestAncestor.map { id: Int =>
			g.nodes.find(_.id == id).get // The ancestor must exist.
		}
	}

	def labelRoute(g: Graph[Node, UnDiEdge])(child: g.NodeT, parent: g.NodeT): g.Path = recursiveLabelRoute(g)(child, parent, Nil)

	@tailrec
	private def recursiveLabelRoute(g: Graph[Node, UnDiEdge])(child: g.NodeT, parent: g.NodeT, path: List[g.NodeT]): g.Path = {
		if( child == parent ) {
			val builder = g.newPathBuilder(child)
			builder ++= path
			builder.result()
		} else {
			val neighborIds = parent.neighbors.map(_.id)
			val eligibleNeighbors = child.label.reverse.find { labelEntries ⇒
				// Get the closest set of eligible neighbors.
				(labelEntries.map(_.id) intersect neighborIds).nonEmpty
			} get // There must be an eligible neighbor.

			val eligibleIds = eligibleNeighbors.map(_.id)
			val bestNeighbor = parent.neighbors.find { neighbor ⇒
				eligibleIds.contains(neighbor.id)
			} get // Assume it exists.

			recursiveLabelRoute(g)(child, bestNeighbor, parent :: path)
		}
	}
}