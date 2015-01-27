package graph

import graph.Units.{Label, Node}

import scala.annotation.tailrec
import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

object Routing {
	def route(g: Graph[Node, UnDiEdge])(from: g.NodeT, to: g.NodeT): g.Path = {
		val path = g.newPathBuilder(from)
		path.add(from)
		// Step 1 Check if they are adjacent
		if(path.add(to)) return path.result()

		// Step 2: Check if there is a node in the triangles around to and from in common
		val tri = Util.triangles(g)
		val fromTriangles = tri.filter { triangle ⇒ triangle.contains(from) }
		val toTriangles = tri.filter { triangle ⇒ triangle.contains(to) }

		// First flatten the triangles into sets of Nodes, then intersect them to find a common node.
		val commonNodes = fromTriangles.flatten intersect toTriangles.flatten
		commonNodes.headOption match {
			case Some(node) ⇒
				val pathBuild = path.add(node)
				assert(pathBuild, "Intermediate node could not be added")
				val finalBuild = path.add(to)
				assert(finalBuild, "Final node could not be added")
				return path.result()
			case None ⇒ // No common node
		}

		splitRoute(g)(from, to, Math.max(from.level, to.level))
	}

	def splitRoute(g: Graph[Node, UnDiEdge])(alpha: g.NodeT, gamma: g.NodeT, k: Int) : g.Path = {
		/**
		 * A NodePath defines a path to the current head.
		 *
		 * This is created to remember from where the path came.
		 * Also two paths are equal when their head is equal, so that only one path remains.
		 * @param path The path stored in this NodePath
		 */
		case class NodePath(path : List[g.NodeT]) {
			override def equals = path.head.equals _
			override def hashCode() = path.head.hashCode()
		}

		val tri = Util.triangles(g)
		@tailrec
		def subSplitRoute(A: Set[NodePath], X: Set[NodePath], k: Int) : g.Path = {
			if(k == 0) {
				val base = g.filter(g.having(node = _.level == 0))
				println(base)

				val shortestPaths = for(pathA ← A;
				    pathX <- X) yield {
					// Assume that the graph is connected
					pathA.path.head.shortestPathTo(pathX.path.head).get
				}
				shortestPaths.reduce((accum, path) => {
					if(accum.length <= path.length) accum
					else path
				})
			} else {
				def parents(nodePath: NodePath) : Set[NodePath] = {
					val parents = nodePath.path.head.diSuccessors.filter(_.level == k-1)
					parents.map((parent) ⇒ NodePath(parent :: nodePath.path))
				}
				val p_kA = A.flatMap(parents)
				val p_kX = X.flatMap(parents)
				val aParentTriangles = p_kA.map(parent ⇒ parent → Util.trianglesAround(g)(parent.path.head)).toMap
				val xParentTriangles = p_kX.map(parent ⇒ parent → Util.trianglesAround(g)(parent.path.head)).toMap

				// Intersect each parent-triangle with each other parent triangle
				val intersectedTriangles = for(aParent ← p_kA;
					xParent ← p_kX;
					aTriangle ← aParentTriangles(aParent);
					xTriangle ← xParentTriangles(xParent)) yield {
						(aParent, xParent) → aTriangle.intersect(xTriangle)
				}
				// If any node was in the intersection, it is a midpoint
				intersectedTriangles.find(_._2.size > 0) match {
					case Some(((pathToMidPoint, pathFromEndPoint), midNodes)) ⇒
						val midNode = midNodes.head
						val totalPath = pathToMidPoint.path.::(midNode).:::(pathFromEndPoint.path.reverse)
						val path = g.newPathBuilder(totalPath.head)

					case None ⇒ // No midpoint was found

				}
			}
		}

		subSplitRoute(Set(NodePath(List(alpha))), Set(NodePath(List(gamma))), k)
	}
}
