package graph.ring

import graph.{ShortestPaths, Util}
import graph.Util.{Layered, ID}
import graph.ring.Units.{Node,LayeredNode,IdNode}
import instrumentation.Metric
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.TraversableOnce
import scalax.collection.GraphEdge._
import scalax.collection.GraphPredef._
import scalax.collection.immutable.Graph

class RoutingSpec extends FlatSpec with Matchers {
	"Ring routing" should "find the shortest path for graphs up to 8 subdivisions" in {
		for(subdivisions ← 1 to 8) {
			println(s"subdivsion: $subdivisions")

			val g = Units.ring(subdivisions)

			val allDistances = ShortestPaths.allDistances(g)
			val graphSize = g.nodes.size
			val nodeMap = g.nodes.toIndexedSeq.sortBy(node => implicitly[ID[Units.Node]].id(node))
			g.nodes.toSeq.combinations(2).foreach {
				case Seq(node1,node2) ⇒
//					val shortestPath = node1.shortestPathTo(node2).get
					val routePath = Routing.route(g)(node1, node2)
					assert(routePath.nodes.head == node1, s"Start node was not equal for ($node1, $node2)")
					assert(routePath.nodes.last == node2, s"End node was not equal for ($node1, $node2)")
					assert(routePath.edges.size == allDistances(node1,node2),
						s"Shortestpath did not equal the optimal path nodes ($node1, $node2).\n${routePath.nodes}")
			}
		}
	}
}
