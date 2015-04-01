package graph

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

import Units.Node

object Paths {

	def all(g: Graph[Node, UnDiEdge]) = {
		val nodeCombinations = g.nodes.toList.combinations(2).toSeq
		val paths = nodeCombinations.map {
			case List(node1,node2) ⇒
				node1.pathTo(node2).toSeq
		}
		paths.flatten
	}
}
