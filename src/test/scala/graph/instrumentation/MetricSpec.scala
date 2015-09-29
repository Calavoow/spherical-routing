package graph.instrumentation

import graph.Util.{ID, Layered}
import graph.sphere.Units.SphereNode.{IdLabel, LayeredLabel}
import graph.{Util, ring, sphere}
import graph.ring.Units.{IdNode,LayeredNode}
import instrumentation.Metric
import org.scalatest.{FlatSpec, Matchers}

class MetricSpec extends FlatSpec with Matchers {
	"A collision on the ring" should "have the right layer number" in {
		val g = ring.Units.ring(3)
		val (n1, n2) = (g.get(0), g.get(16))
		val (v1, v2) = (g.get(1), g.get(17))

		val graphSize = g.nodes.size
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node ⇒ implicitly[ID[ring.Units.Node]].id(node))
		val route1 = ring.Routing.route(g)(n1, n2)
		val route2 = ring.Routing.route(g)(v1, v2)
		val collidingEdge = Metric.collisionEdge(g)(List(route1, route2))
		assert(collidingEdge.isDefined)
		val layer = Layered.edgeLayer[ring.Units.Node](collidingEdge.get.toOuter).layer(collidingEdge.get.toOuter, 4)
		layer should equal(0)
	}

	it should "only occur when there is a collision" in {
		val g = ring.Units.ring(3)
		val n1 = g.nodes.find(_ == 0).get
		val n2 = g.nodes.find(_ == 16).get
		val v1 = g.nodes.find(_ == 1).get
		val v2 = g.nodes.find(_ == 17).get

		val graphSize = g.nodes.size
		val nodeMap = g.nodes.toIndexedSeq.sortBy(node ⇒ implicitly[ID[ring.Units.Node]].id(node))

		val route1 = ring.Routing.route(g)(n1, n2)
		val route2 = ring.Routing.route(g)(v1, v2)
		val collidingEdge = Metric.collisionEdge(g)(List(route1, route2))
		assert(collidingEdge.isDefined)
		println(collidingEdge)
	}

	"A collision on the sphere" should "have the right layer number" in {
		val g0 = sphere.Units.triangle
		val g = sphere.SphereApproximation.repeatedSubdivision(g0).drop(1).next()
		val n1 = g.nodes.find(_.id == 0).get
		val n2 = g.nodes.find(_.id == 1).get
		val v1 = g.nodes.find(_.id == 5).get
		val v2 = g.nodes.find(_.id == 1).get

//		val graphSize = g.nodes.size
//		val nodeMap = g.nodes.toIndexedSeq.sortBy(_.id)
//		val pathMap = Util.allShortestPaths(g0)
//		val sphereRouter = sphere.Routing.sphereRouter(g0)(pathMap)

		val route1 = g.newPathBuilder(n1).+=(n2).result()
		val route2 = g.newPathBuilder(v1).+=(n1).+=(v2).result()
		val collidingEdge = Metric.collisionEdge(g)(List(route1, route2))
		assert(collidingEdge.isDefined)
		val layer = Layered.edgeLayer[sphere.Units.Node](collidingEdge.get.toOuter).layer(collidingEdge.get.toOuter, 2)
		layer should equal(0)
	}

	it should "have the right layer in layer 0" in {
		val g0 = sphere.Units.triangle
		val g = sphere.SphereApproximation.repeatedSubdivision(g0).drop(2).next()
		println(g.nodes)
		val n1 = g.nodes.find(_.id == 4).get
		val n2 = g.nodes.find(_.id == 9).get
		val v1 = g.nodes.find(_.id == 9).get
		val v2 = g.nodes.find(_.id == 12).get

		val graphSize = g.nodes.size
		val nodeMap = g.nodes.toIndexedSeq.sortBy(_.id)

		val route1 = sphere.Routing.route(g)(n1, n2)
		val route2 = sphere.Routing.route(g)(v1, v2)
		val collidingEdge = Metric.collisionEdge(g)(List(route1, route2))
		assert(collidingEdge.isDefined)
		val layeredEdge = Layered.edgeLayer[sphere.Units.Node](collidingEdge.get.toOuter)
		val outer = collidingEdge.get.toOuter
		val layer = layeredEdge.layer(outer, 3)
		layer should equal(2)
	}
}
