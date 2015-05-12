package graph.ring

import org.scalatest.{FlatSpec, Matchers}

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class RoutingSpec extends FlatSpec with Matchers {
	"Route" should "find the shortest path for graphs up to 7 subdivisions" in {
		for(subdivisions ← 1 to 7) {
			println(s"subdivsion: $subdivisions")
			val g = Units.ring(subdivisions)
			val graphSize = g.nodes.size
			g.nodes.toSeq.combinations(2).foreach {
				case Seq(node1,node2) ⇒
					val shortestPath = node1.shortestPathTo(node2).get
					val routePath = Routing.route(g, graphSize)(node1, node2)
					assert(routePath.nodes.size == shortestPath.nodes.size, s"Shortestpath was different than optimal route for nodes ($node1, $node2).\n${shortestPath.nodes}\n${routePath.nodes}")
			}
		}
	}
}
