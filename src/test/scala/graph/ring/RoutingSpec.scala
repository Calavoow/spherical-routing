package graph.ring

import graph.Util
import graph.Util.{Layered, ID}
import graph.ring.Units.{Node,LayeredNode,IdNode}
import instrumentation.Metric
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.TraversableOnce
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

	"A collision" should "have the right layer number" in {
		val g = Units.ring(3)
		val (n1, n2) = (g.get(0), g.get(16))
		val (v1, v2) = (g.get(1), g.get(17))

		val graphSize = g.nodes.size
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node ⇒ implicitly[ID[Units.Node]].id(node))
		val route1 = Routing.route(g, graphSize)(n1, n2, nodeMap)
		val route2 = Routing.route(g, graphSize)(v1, v2, nodeMap)
		val collidingEdge = Metric.collisionEdge(g)(List(route1, route2))
		assert(collidingEdge.isDefined)
		val layer = Layered.edgeLayer[Node](collidingEdge.get.toOuter).layer(collidingEdge.get.toOuter, 4)
		layer should equal(3)
	}
}
