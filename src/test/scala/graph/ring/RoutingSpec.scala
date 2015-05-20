package graph.ring

import graph.Util
import graph.Util.ID
import graph.ring.Units.IdNode
import org.scalatest.{FlatSpec, Matchers}

import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class RoutingSpec extends FlatSpec with Matchers {
	"Ring routing" should "find the shortest path for graphs up to 5 subdivisions" in {
		for(subdivisions ← 1 to 7) {
			println(s"subdivsion: $subdivisions")
			val g = Units.ring(subdivisions)
			val graphSize = g.nodes.size
			val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[Units.Node]].id(node))
			g.nodes.toSeq.combinations(2).foreach {
				case Seq(node1,node2) ⇒
					val shortestPath = node1.shortestPathTo(node2).get
					val routePath = Routing.route(g, graphSize)(node1, node2, nodeMap)
					assert(routePath.edges.size == shortestPath.edges.size,
						s"Shortestpath did not equal the optimal path nodes ($node1, $node2).\n${shortestPath.nodes}\n${routePath.nodes}")
			}
		}
	}
}
