package graph

import graph.Units.{Label, Node}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import Util.triangles

object Routing {
	def route(g: Graph[Node, UnDiEdge])(from: g.NodeT, to: g.NodeT): g.Path = {
		val path = g.newPathBuilder(from)
		path.add(from)
		// Step 1 Check if they are adjacent
		if(path.add(to)) return path.result()

		// Step 2: Check if there is a node in the triangles around to and from in common
		val tri = triangles(g)
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

		null
	}
}
