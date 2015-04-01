package graph

import graph.Units._
import org.scalatest.{Matchers, FlatSpec}

import scalax.collection.immutable.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._

class RoutingSpec extends FlatSpec with Matchers {
/*
	"Route" should "find a one-hop path" in {
		val g = Graph[Node, UnDiEdge](
			Label(0,1) ~ Label(0,2),
			Label(0,2) ~ Label(0,3),
			Label(0,3) ~ Label(0,1),
			Label(0,2) ~ Label(0,4),
			Label(0,4) ~ Label(0,3),
			Label(0,1) ~ Label(0,5)
		)

		val route = Routing.route(g)(g.get(Label(0,1)), g.get(Label(0,5)))

		route.nodes.toList should be(List(Label(0,1), Label(0,5)))
		route.edges.toList should be(List(Label(0,1) ~ Label(0,5)))
	}

	it should "find a two-hop-path" in {
		val g = Graph[Node, UnDiEdge](
			Label(0,1) ~ Label(0,2),
			Label(0,2) ~ Label(0,3),
			Label(0,3) ~ Label(0,1),
			Label(0,2) ~ Label(0,4),
			Label(0,4) ~ Label(0,3),
			Label(0,1) ~ Label(0,5)
		)

		val route = Routing.route(g)(g.get(Label(0,1)), g.get(Label(0,4)))

		route.nodes.toList should (equal (List(Label(0,1), Label(0,3), Label(0,4)))
			or equal (List(Label(0,1), Label(0,2), Label(0,4))))
		route.edges.toList should (equal (List(Label(0,1) ~ Label(0,3), Label(0,3) ~ Label(0,4)))
			or equal (List(Label(0,1) ~ Label(0,2), Label(0,2) ~ Label(0,4))))
	}
*/
}
