package graph

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import Units.Node

object Paths {

	def all(g: Graph[Node, UnDiEdge]) : Iterator[g.Path] = {
		g.nodes.toList.combinations(2).map {
			case List(node1,node2) ⇒
				node1.shortestPathTo(node2).get
		}
	}
}
